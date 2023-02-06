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
      val DefaultPort = 8999
      val DefaultDirectoryIndexes = immutable.Set("index.html","index.htm","index.rss","index.xml")
      val Default = Dynamic(DefaultPort, DefaltServerInterpreterOptions, DefaultDirectoryIndexes)
      given Config.Dynamic = Default
    case class Dynamic( port: Int, serverInterpreterOptions: ZioHttpServerOptions[Any], directoryIndexes : immutable.Set[String] )
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

  def serve(site: ZTSite)(using cfg: Config.Dynamic) =
    def buildApp(endpointSource: ZTEndpointBinding.Source): HttpApp[Any, Throwable] =
      val endpointBindings = endpointSource.endpointBindings
      //val endpoints = endpointBindings.map(_.ztServerEndpoint)
      if (endpointBindings.isEmpty) throw new NoEndpointsDefined(s"No endpoints defined to serve from site for ${site.sitePath}.")

      // we need to find the directories associated with directory indexes, and create bindings for those
      val directoryIndexDirectoryBindingsByIndexBinding =
        endpointBindings.map( binding => endpointStaticallyGenerableFilePath( binding ) ).zip(endpointBindings)
          .collect { case (Some( path ), binding ) => (path, binding) }
          .filter { case (path, _) =>
            val elements = path.elements
            elements.nonEmpty && cfg.directoryIndexes(elements.last)
          }
          .map { case (dirIndexPath, fullIndexBinding) =>
            val dirBinding =
              val newServerRootedPath = dirIndexPath.parent.resolve("") // we want the empty string in this dir, not a file in the parent
              val newSiteRootedPath   = site.siteRootedPath(newServerRootedPath)
              staticallyGenerableZTEndpointBindingWithNewServerRootedPath(newServerRootedPath,site,fullIndexBinding)
            val redirectBinding =
              val fromServerRootedPath = dirIndexPath.parent // here we want to set handling for parent as file, no empty string to get us in the directory
              val toServerRootedPath = dirIndexPath.parent.resolve("") // we want to go to parent as directory index, basically a file with empty string as name
              redirectZTEndpointBinding(fromServerRootedPath, toServerRootedPath, site)
            ( fullIndexBinding, Tuple2(redirectBinding, dirBinding) )
          }
          .toMap

      // then we need to place the directory bindings with the index bindings to preserve
      // the intended priority of resolution
      val enrichedEndpointBindings =
        endpointBindings.map { origBinding =>
          directoryIndexDirectoryBindingsByIndexBinding.get(origBinding) match
            case None                                   => Seq( origBinding )
            case Some( (redirectBinding, dirBinding ) ) => Seq( origBinding, redirectBinding, dirBinding )
        }
        .flatten

      val enrichedEndpoints = enrichedEndpointBindings.map(_.ztServerEndpoint)

      enrichedEndpoints.foreach( zse => println(zse.show) )

      def toHttp(endpoint: ZTServerEndpoint): Http[Any, Throwable, Request, Response] = ZioHttpInterpreter(cfg.serverInterpreterOptions).toHttp(endpoint)

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
                 .valueName("<path1>,<path2>,...")
            ),
          cmd(Config.Command.serve.toString)
            .text("serve site dynamically")
            .action((_, cfg) => cfg.copy(command = Config.Command.serve))
            .children(
              opt[Int]('p', "port")
                .action( (x, cfg) => cfg.copy( cfgDynamic = cfg.cfgDynamic.copy(port = x) ) )
                .text("the port on which to serve HTTP"),
              opt[Unit]("verbose")
                .action( (_, cfg) => cfg.copy( cfgDynamic = cfg.cfgDynamic.copy(serverInterpreterOptions = Config.Dynamic.VerboseServerInterpreterOptions) ) )
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
                 .valueName("<path1>,<path2>,..."),
              opt[Int]('p', "port")
                .action( (x, cfg) => cfg.copy( cfgDynamic = cfg.cfgDynamic.copy(port = x) ) )
                .text("the port on which to serve HTTP"),
              opt[Unit]("verbose")
                .action( (_, cfg) => cfg.copy( cfgDynamic = cfg.cfgDynamic.copy(serverInterpreterOptions = Config.Dynamic.VerboseServerInterpreterOptions) ) )
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

    override def run = runTask.catchSome{ case _ : BadCommandLine => ZIO.unit }

    val runTask =
      for
        args <- getArgs
        cfg  <- ZIO.attempt( config(args.toArray) )
        _    <- work(cfg).debug.exitCode
      yield ()
  end Main

  trait Composite extends ZTSite with Site.Composite:
    def endpointBindingSources : immutable.Seq[ZTEndpointBinding.Source]
    def endpointBindings : immutable.Seq[ZTEndpointBinding] = endpointBindingSources.flatMap( _.endpointBindings )
  end Composite

trait ZTSite extends Site with ZTEndpointBinding.Source
