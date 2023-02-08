package unstatic.ztapir

import scala.collection.*

import sttp.tapir.ztapir.*
import sttp.tapir.server.interceptor.log.DefaultServerLog
import sttp.tapir.server.ziohttp.{ZioHttpInterpreter, ZioHttpServerOptions}
import zio.http.{Http, HttpApp, Request, Response}
import zio.http.{Server, ServerConfig}
import zio.*

import unstatic.*, UrlPath.*

import java.nio.file.Path as JPath

object ZTSite:
  object Config:
    object Dynamic:
      val DefaultPort = 8999
      val DefaultVerbose = false
      val DefaultDirectoryIndexes = immutable.Set("index.html","index.htm","index.rss","index.xml")
      val Default = Dynamic(DefaultPort, DefaultVerbose, DefaultDirectoryIndexes)
      given Config.Dynamic = Default
    case class Dynamic( port: Int, verbose: Boolean, directoryIndexes : immutable.Set[String] )
    object Static:
      val Default = Config.Static( JPath.of("public"), Nil )
      given Config.Static = Default
    case class Static( generateTo : JPath, noGenPrefixes : scala.Seq[Rooted] = Nil )
    enum Command:
      case gen, serve, hybrid
  case class Config(
    command    : Config.Command = Config.Command.gen,
    cfgStatic  : Config.Static  = Config.Static.Default,
    cfgDynamic : Config.Dynamic = Config.Dynamic.Default,
  )

  val VerboseServerInterpreterOptions: ZioHttpServerOptions[Any] =
  // modified from https://github.com/longliveenduro/zio-geolocation-tapir-tapir-starter/blob/b79c88b9b1c44a60d7c547d04ca22f12f420d21d/src/main/scala/com/tsystems/toil/Main.scala
    ZioHttpServerOptions
      .customiseInterceptors
      .serverLog(
        DefaultServerLog[Task](
          doLogWhenReceived = msg => ZIO.succeed(println(msg)),
          doLogWhenHandled = (msg, error) => ZIO.succeed(error.fold(println(msg))(err => println(s"msg: ${msg}, err: ${err}"))),
          doLogAllDecodeFailures = (msg, error) => ZIO.succeed(error.fold(println(msg))(err => println(s"msg: ${msg}, err: ${err}"))),
          doLogExceptions = (msg: String, exc: Throwable) => ZIO.succeed(println(s"msg: ${msg}, exc: ${exc}")),
          noLog = ZIO.unit
        )
      )
      .options
  val DefaltServerInterpreterOptions: ZioHttpServerOptions[Any] = ZioHttpServerOptions.default.widen[Any]
  def interpreterOptions( verbose : Boolean ) = if verbose then VerboseServerInterpreterOptions else DefaltServerInterpreterOptions

  def serve(site: ZTSite)(using cfg: Config.Dynamic) =
    def buildApp(endpointSource: ZTEndpointBinding.Source): HttpApp[Any, Throwable] =
      val endpointBindings = endpointSource.endpointBindings
      //val endpoints = endpointBindings.map(_.ztServerEndpoint)
      if (endpointBindings.isEmpty) throw new NoEndpointsDefined(s"No endpoints defined to serve from site for ${site.sitePath}.")

      // we need to find the directories associated with directory indexes, and create bindings for those
      val directoryIndexDirectoryBindingByIndexBinding =
        endpointBindings.map( binding => endpointStaticallyGenerableFilePath( binding ) ).zip(endpointBindings)
          .collect { case (Some( path ), binding ) => (path, binding) }
          .filter { case (path, _) =>
            val elements = path.elements
            elements.nonEmpty && cfg.directoryIndexes(elements.last)
          }
          .map { case (dirIndexPath, fullIndexBinding) =>
/*
            val dirBinding =
              val newServerRootedPath = dirIndexPath.parent // parent will represent the directory
              staticallyGenerableZTEndpointBindingWithNewServerRootedPath(newServerRootedPath,site,fullIndexBinding)
            val redirectBinding =
              val fromServerRootedPath = dirIndexPath.parent.asNotDir // here we want to set handling for parent as file, no empty string to get us into the directory
              val toServerRootedPath = dirIndexPath.parent // we want to go to parent as directory index
              redirectZTEndpointBinding(fromServerRootedPath, toServerRootedPath, site)
            ( fullIndexBinding, Tuple2(redirectBinding, dirBinding) )
*/
/*
            val redirectBinding =
              val fromServerRootedPath = dirIndexPath.parent.asNotDir
              redirectOrServerDirectoryIndexZTEndpointBinding( fromServerRootedPath, site )
            ( fullIndexBinding, redirectBinding )
*/
            val redirectBinding =
              val fromServerRootedPath = dirIndexPath.parent
              val toServerRootedPath = dirIndexPath
              redirectZTEndpointBinding( fromServerRootedPath, toServerRootedPath, site )
            ( fullIndexBinding, redirectBinding )
          }
          .toMap

      // then we need to place the directory bindings with the index bindings to preserve
      // the intended priority of resolution
      val enrichedEndpointBindings =
        endpointBindings.map { origBinding =>
          directoryIndexDirectoryBindingByIndexBinding.get(origBinding) match
            case None                    => Seq( origBinding )
            case Some( redirectBinding ) => Seq( origBinding, redirectBinding )
        }
        .flatten

      val enrichedEndpoints = enrichedEndpointBindings.map(_.ztServerEndpoint)

      if cfg.verbose then
        scala.Console.err.println("Endpoints to serve:")
        enrichedEndpoints.foreach( zse => scala.Console.err.println( "  - " + zse.show) )

      def toHttp(endpoint: ZTServerEndpoint): Http[Any, Throwable, Request, Response] = ZioHttpInterpreter(interpreterOptions(cfg.verbose)).toHttp(endpoint)

      enrichedEndpoints.tail.foldLeft(toHttp(enrichedEndpoints.head))((accum, next) => accum ++ toHttp(next))

    val configLayer = ServerConfig.live(ServerConfig.default.port(cfg.port))
    val server =
      for
        app <- ZIO.attempt(buildApp(site))
        _   <- Console.printLineError(s"Beginning HTTP Service on port ${cfg.port}.")
        svr <- Server.serve(app)
      yield svr
    server.provide(configLayer, Server.live)

  def generate( site : ZTSite )(using cfg : Config.Static) =
    ZTStaticGen.generateZTSite( site, cfg.generateTo, cfg.noGenPrefixes )

  abstract class Main(site: ZTSite, executableName : String = "unstatic") extends ZIOAppDefault:
    def config( args : Array[String] ) : Config =
      import scopt.OParser
      val builder = OParser.builder[Config]
      val parser1 =
        import builder._
        OParser.sequence(
          programName(executableName),
          cmd(Config.Command.gen.toString)
            .text("generate fully static site")
            .action((_, c) => c.copy(command = Config.Command.gen))
            .children(
               opt[java.io.File]('o', "out")
                 .action((x, cfg) => cfg.copy( cfgStatic = cfg.cfgStatic.copy(generateTo = x.toPath) ) )
                 .valueName("<dir>")
                 .text("the output directory, into which the site will be generated"),
               opt[scala.Seq[String]]("no-gen-prefixes")
                 .action((x, cfg) => cfg.copy( cfgStatic = cfg.cfgStatic.copy(noGenPrefixes = x.map(Rooted.parse)) ) )
                 .text("prefixes for paths that should be ignored (skipped) for static generation")
                 .valueName("<path1>,<path2>,..."),
            ),
          cmd(Config.Command.serve.toString)
            .text("serve site dynamically")
            .action((_, cfg) => cfg.copy(command = Config.Command.serve))
            .children(
              opt[Int]('p', "port")
                .action( (x, cfg) => cfg.copy( cfgDynamic = cfg.cfgDynamic.copy(port = x) ) )
                .valueName("<port-number>")
                .text("the port on which to serve HTTP"),
              opt[Unit]("verbose")
                .action( (_, cfg) => cfg.copy( cfgDynamic = cfg.cfgDynamic.copy(verbose = true) ) )
                .text("emit verbose debugging output to stderr"),
               opt[scala.Seq[String]]("directory-indexes")
                 .action( (x, cfg) => cfg.copy( cfgDynamic = cfg.cfgDynamic.copy(directoryIndexes = x.toSet) ) )
                 .text("names that can represent content of parent dir path")
                 .valueName("index.html,index.htm,..."),
            ),
          cmd(Config.Command.hybrid.toString)
            .text("generate partial site and serve rest dynamically")
            .action((_, cfg) => cfg.copy(command = Config.Command.serve))
            .children(
               opt[java.io.File]('o', "out")
                 .action( (x, cfg) => cfg.copy( cfgStatic = cfg.cfgStatic.copy(generateTo = x.toPath) ) )
                 .valueName("<dir>")
                 .text("the output directory, into which the site will be generated"),
               opt[scala.Seq[String]]("no-gen-prefixes")
                 .action( (x, cfg) => cfg.copy( cfgStatic = cfg.cfgStatic.copy(noGenPrefixes = x.map(Rooted.parse)) ) )
                 .text("prefixes for paths that should be ignored (skipped) for static generation")
                 .valueName("<path1>,<path2>,..."),
               opt[scala.Seq[String]]("directory-indexes")
                 .action( (x, cfg) => cfg.copy( cfgDynamic = cfg.cfgDynamic.copy(directoryIndexes = x.toSet) ) )
                 .text("names that can represent content of parent dir path")
                 .valueName("index.html,index.htm,..."),
               opt[Int]('p', "port")
                .action( (x, cfg) => cfg.copy( cfgDynamic = cfg.cfgDynamic.copy(port = x) ) )
                .valueName("<port-number>")
                .text("the port on which to serve HTTP"),
              opt[Unit]("verbose")
                .action( (_, cfg) => cfg.copy( cfgDynamic = cfg.cfgDynamic.copy(verbose = true) ) )
                .text("emit verbose debugging output to stderr"),
            )
       )
      OParser.parse(parser1, args, Config()) match
        case Some(cfg) => cfg
        case _ => throw new BadCommandLine("Bad command line options provided: " + args.mkString(" "))

    def work( cfg : Config ) : Task[ZTStaticGen.Result] | Task[Unit] =
      val genTask   = generate(site)(using cfg.cfgStatic)
      val serveTask = serve(site)(using cfg.cfgDynamic)
      cfg.command match
        case Config.Command.gen    => genTask
        case Config.Command.serve  => serveTask
        case Config.Command.hybrid =>
          for
            staticResult <- genTask
            _            <- serveTask
          yield staticResult

    def printEndpointsWithHeader(header : String, endpoints : immutable.Seq[Rooted]) : Task[Unit] =
      if endpoints.isEmpty then
        ZIO.unit
      else
        for
          _ <- Console.printLine(header)
          _ <- ZIO.foreach(endpoints.map(_.toString).to(immutable.SortedSet))( endpoint => Console.printLine(s" \u27A3 ${endpoint}"))
        yield ()

    def printCopiedWithHeader(header : String, staticEndpoints : immutable.Seq[StaticLocationBinding]) : Task[Unit] =
      val sortedEndpoints = staticEndpoints.sortBy( _.siteRootedPath.toString )
      if staticEndpoints.isEmpty then
        ZIO.unit
      else
        for
          _ <- Console.printLine(header)
          _ <- ZIO.foreach(sortedEndpoints)( endpoint => Console.printLine(s" \u27A3 ${endpoint.siteRootedPath} <- ${endpoint.source}"))
        yield ()


    def reportResult( result : ZTStaticGen.Result ) : Task[Unit] =
      val ZTStaticGen.Result( generated, copied, ignored, ungenerable ) = result
      for
        _ <- printEndpointsWithHeader("Endpoints generated:", generated)
        _ <- printCopiedWithHeader("Endpoints copied from static locations:", copied)
        _ <- printEndpointsWithHeader("Endpoints ignored by request:", ignored)
        _ <- printEndpointsWithHeader("Endpoints with ungenerable definitions skipped:", ungenerable)
      yield ()

    def reportMaybeResult( mbr : ZTStaticGen.Result | Unit ) : Task[Unit] =
      mbr match
        case result : ZTStaticGen.Result => reportResult(result)
        case _                           => ZIO.unit

    override def run = runTask.catchSome{ case _ : BadCommandLine => ZIO.unit }.exitCode

    val runTask =
      for
        args <- getArgs
        cfg <- ZIO.attempt(config(args.toArray))
        mbr <- work(cfg)
        _   <- reportMaybeResult(mbr)
      yield ()
  end Main

  trait Composite extends ZTSite with Site.Composite:
    def endpointBindingSources : immutable.Seq[ZTEndpointBinding.Source]
    def endpointBindings : immutable.Seq[ZTEndpointBinding] = endpointBindingSources.flatMap( _.endpointBindings )
  end Composite

trait ZTSite extends Site with ZTEndpointBinding.Source
