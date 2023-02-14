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

object ZTMain:
  object Config:
    object List:
      val Default = List( false, None )
    case class List(allIdentifiers : Boolean, substringToMatch : Option[String])
    object Dynamic:
      enum IndexStyle:
        case RedirectToIndex, RedirectToSlash
      val DefaultPort = 8999
      val DefaultVerbose = false
      val DefaultDirectoryIndexes = immutable.Set("index.html","index.htm","index.rss","index.xml")
      val DefaultIndexStyle = IndexStyle.RedirectToIndex
      val Default = Dynamic(DefaultPort, DefaultVerbose, DefaultDirectoryIndexes, DefaultIndexStyle)
      given Config.Dynamic = Default
    case class Dynamic( port: Int, verbose: Boolean, directoryIndexes : immutable.Set[String], indexStyle : Dynamic.IndexStyle )
    object Static:
      val Default = Config.Static( JPath.of("public"), Nil )
      given Config.Static = Default
    case class Static( generateTo : JPath, noGenPrefixes : scala.Seq[Rooted] = Nil )
    enum Command:
      case gen, serve, hybrid, list
  case class Config(
    command    : Config.Command = Config.Command.gen,
    cfgList    : Config.List    = Config.List.Default,
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
        endpointBindings
          .collect { case g : ZTEndpointBinding.Generable => g }
          .map( g => Tuple2(g.siteRootedPath, g ) )
          .filter { case (path, _) =>
            val elements = path.elements
            elements.nonEmpty && cfg.directoryIndexes(elements.last)
          }
          .map { case (siteRootedDirIndexPath, fullIndexBinding) =>
            val serverRootedDirIndexPath = site.serverRootedPath(siteRootedDirIndexPath)

            val redirectBindings =
              cfg.indexStyle match
                case Config.Dynamic.IndexStyle.RedirectToIndex =>
                  Seq( redirectZTEndpointBinding( serverRootedDirIndexPath.parent, serverRootedDirIndexPath, site ) )
                case Config.Dynamic.IndexStyle.RedirectToSlash =>
                  val serverRootedDirIndexPathParent = serverRootedDirIndexPath.parent
                  val redirectEndpointBinding =
                    val asLeaf = serverRootedDirIndexPathParent.asLeaf
                    val ztServerEndpoint =
                      endpointForFixedPath( asLeaf )
                        .in( noTrailingSlash )
                        .out( redirectOutputs(serverRootedDirIndexPathParent) )
                        .zServerLogic( UnitUnitUnitLogic )
                        .glitchWiden
                    ZTEndpointBinding.Generic( site.siteRootedPath(asLeaf), ztServerEndpoint, UnitThrowableUnitLogic, NoIdentifiers )
                  val slashEndpointBinding =
                    val basicEndpoint =
                      endpointForFixedPath(serverRootedDirIndexPathParent)
                        .errorOut(stringBody(CharsetUTF8))
                        .out(header(sttp.model.Header.contentType(fullIndexBinding.contentType)))
                    fullIndexBinding match
                      case sg : ZTEndpointBinding.StringGenerable =>
                        val coreLogic = (_:Unit) => sg.generator
                        val ztse =
                          val ct = fullIndexBinding.contentType
                          val htmlUtf8 = ct.mainType == "text" && ct.subType == "html" && sg.charset == CharsetUTF8
                          basicEndpoint
                            .out(if htmlUtf8 then htmlBodyUtf8 else stringBody(sg.charset))
                            .zServerLogic( errMapped(coreLogic) )
                            .glitchWiden
                        ZTEndpointBinding.Generic(site.siteRootedPath(serverRootedDirIndexPathParent), ztse, coreLogic, NoIdentifiers)
                      case otherGenerable =>
                        val coreLogic = (_:Unit) => otherGenerable.bytesGenerator
                        val ztse =
                          basicEndpoint
                            .out( byteArrayBody )
                            .zServerLogic(errMapped(coreLogic.andThen( _.map(_.toArray) )))
                            .glitchWiden
                        ZTEndpointBinding.Generic(site.siteRootedPath(serverRootedDirIndexPathParent), ztse, coreLogic, NoIdentifiers)
                  Seq( redirectEndpointBinding, slashEndpointBinding )
            ( fullIndexBinding, redirectBindings )
          }
          .toMap

      // then we need to place the directory bindings with the index bindings to preserve
      // the intended priority of resolution
      val enrichedEndpointBindings =
        endpointBindings.flatMap { origBinding =>
          origBinding match
            case gen : ZTEndpointBinding.Generable =>
              directoryIndexDirectoryBindingByIndexBinding.get(gen) match
                case Some( redirectBindings ) => redirectBindings :+ gen
                case None                     => Seq( gen )
            case _ => Seq( origBinding )
        }

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
    scribe.trace( s"generate( ${site} )")
    ZTStaticGen.generateZTSite( site, cfg.generateTo, cfg.noGenPrefixes )

  private def matchesSubstring( binding : AnyBinding, substr : String ) : Boolean =
    val srpLc = binding.siteRootedPath.toString().toLowerCase
    if srpLc.indexOf(substr) >= 0 then
      true
    else
      binding.identifiers.view.map( _.toLowerCase ).exists(str => str.indexOf(substr) >= 0)

  private def printIdentifierLine( id : String ) = Console.printLine("     \u27A3 " + id)
  private def printIdentifiers( ids : immutable.Set[String], site : ZTSite, cfg : Config.List ) : Task[Unit] =
    val byLenUids = ids.toVector.sortBy( s => (s.length, s) ).filter(id => !site.duplicateIdentifiers(id))
    if cfg.allIdentifiers then
      Console.printLine( "    identifiers (unique):" ) *> ZIO.foreachDiscard( byLenUids.map( printIdentifierLine ) )(identity)
    else
      byLenUids.headOption match
        case Some(identifier) => Console.printLine(s"    uid: ${identifier}")
        case None             => Console.printLine( "    uid: <no-unique-identifiers>" )

  private def printInfoByType( site : ZTSite, binding : AnyBinding ) : Task[Unit] =
    binding match
      case slb : StaticLocationBinding =>
        Console.printLine("  Copy-on-gen filesystem location.") *> Console.printLine(s"    source-dir: ${slb.source}")
      case fsd : ZTEndpointBinding.FromStaticDirectory =>
        Console.printLine("  Static HTTP service endpoint.") *> Console.printLine(s"    source-dir: ${fsd.dir}")
      case sg : ZTEndpointBinding.StringGenerable =>
        Console.printLine(s"  String endpoint of type '${sg.contentType}'.") *>
        Console.printLine("  Static generation and HTTP service.") *>
          sg.mediaDirSiteRooted.fold(ZIO.unit) { mdsr =>
            Console.printLine(s"    media-dir: ${mdsr}") *>
            site.enforceUserContentFrom.fold(ZIO.unit) { userRoot =>
              val mediaDir = userRoot.resolve(mdsr.unroot.toString())
              Console.printLine("    media-dir (absolute): ") *>
              Console.printLine(s"      ${mediaDir.toAbsolutePath.toString}")
            }
          }
      case bg : ZTEndpointBinding.BytesGenerable =>
        Console.printLine(s"  Binary endpoint of type '${bg.contentType}'.") *> Console.printLine("  Static generation and HTTP service.")
      case generic : ZTEndpointBinding.Generic[?,?] =>
        Console.printLine("  Generic endpoint. No further information.")
      case other =>
        Console.printLine(s"  Unexpected endpoint type: ${other}")

  def list( site : ZTSite )(using cfg : Config.List) : Task[Unit] =
    val bindings =
      cfg.substringToMatch match
        case Some(substr) => site.allBindings.filter(binding => matchesSubstring(binding, substr))
        case None         => site.allBindings
    val bindingsPrinters =
      bindings.map { binding =>
        for
          _ <- Console.printLine(s"Location: ${binding.siteRootedPath.toString()}")
          _ <- printInfoByType( site, binding)
          _ <- printIdentifiers( binding.identifiers, site, cfg )
        yield ()
    }
    ZIO.foreachDiscard(bindingsPrinters)(identity)

abstract class ZTMain(site: ZTSite, executableName : String = "unstatic") extends ZIOAppDefault:
  import ZTMain.*
  def config( args : Array[String] ) : Config =
    import scopt.OParser
    val builder = OParser.builder[Config]
    val parser1 =
      import builder._
      OParser.sequence(
        programName(executableName),
        cmd(Config.Command.list.toString)
          .text("list and show information about endpoints")
          .action((_, cfg) => cfg.copy(command = Config.Command.list))
          .children(
             opt[Unit]('a', "all-identfiers")
              .action( (_, cfg) => cfg.copy( cfgList = cfg.cfgList.copy(allIdentifiers = true) ) )
              .text("Display all unique identifiers for each endpoint."),
             opt[String]('f', "filter-by-substring")
              .action( (x, cfg) => cfg.copy( cfgList = cfg.cfgList.copy(substringToMatch = Some(x) ) ) )
              .valueName("<substring>")
              .text("Restrict output to endpoints with path or identifiers containing substring."),
          ),
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
             opt[String]("index-redirect-to")
               .validate { x =>
                  if (x == "slash" || x == "index") then success
                  else failure("--index-redirect-to [slash|index] only" )
               }
               .action { (x, cfg) =>
                 val style =
                   if x == "slash" then
                     Config.Dynamic.IndexStyle.RedirectToSlash
                   else
                     Config.Dynamic.IndexStyle.RedirectToIndex
                 cfg.copy(cfgDynamic = cfg.cfgDynamic.copy(indexStyle = style))
               }
               .text("paths to directories with indexes should redirect to full index, or only to the directory trailing slash?")
               .valueName("[slash|index]"),
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
             opt[String]("index-redirect-to")
               .validate { x =>
                  if (x == "slash" || x == "index") then success
                  else failure("--index-redirect-to [slash|index] only" )
               }
               .action { (x, cfg) =>
                 val style =
                   if x == "slash" then
                     Config.Dynamic.IndexStyle.RedirectToSlash
                   else
                     Config.Dynamic.IndexStyle.RedirectToIndex
                 cfg.copy(cfgDynamic = cfg.cfgDynamic.copy(indexStyle = style))
               }
               .text("paths to directories with indexes should redirect to full index, or only to the directory trailing slash?")
               .valueName("[slash|index]"),
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
    scribe.trace( s"work( ${cfg} )")
    val genTask   = generate(site)(using cfg.cfgStatic)
    val serveTask = serve(site)(using cfg.cfgDynamic)
    val listTask  = list(site)(using cfg.cfgList)
    cfg.command match
      case Config.Command.list   => listTask
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

  /*
  // we were trying to debug a silent failure issue, hypothesized that
  // a fatal error was the issue. it was not, fatal errors are properly
  // logged, the issue is that failures represented by Cause are not
  // caught by tapError(...), so under some circumstances the app failed
  // silently and inscrutably.

  def logFatal( t : Throwable ) : Nothing =
    scribe.error("Fatal error!!!", t)
    throw t

  // modified from https://github.com/poslegm/munit-zio/blob/b6708da58ba1963166fff404c6209c80d3f3f775/core/src/main/scala/munit/ZRuntime.scala
  override def runtime: Runtime[Any] =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.unsafe.fromLayer(Runtime.setReportFatal {
        case cause =>
          scribe.error("Fatal error!")
          cause.printStackTrace
          throw cause
      })
    }
  */


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

  def reportFinalThrowable( t : Throwable ) = ZIO.attempt {
    t match
      case ur : UnresolvedReference =>
        val absInsert = ur.absolute.fold("")(abs =>
          s"${scala.util.Properties.lineSeparator}    Expected destination (absolute): ${abs}"
        )
        val msg =
          s"""|Unresolved Reference: ${ur.reference}
              |  ${ur.explanation}
              |    Source: ${ur.source}${absInsert}
              |""".stripMargin
        scala.Console.err.println(msg)
      case other =>
        other.printStackTrace()
  }

  override def run =
    runTask
      .tapDefect { cause =>
        scribe.error("Logging failures (but ugh, via ZIO.logCause(...)!!!")
        // XXX: If I'm logging with scribe, I need some way to decode the
        // cause into an exhaustive dump, rather than relying inconsistently
        // on ZIO.logCause.
        //
        // But for now I want to be sure that failures are never silent.
        // Didn't work:
        //   ZIO.attempt(cause.failures.foreach(f => scribe.error("Failure!", f)))
        ZIO.logCause(cause)
      }
      //.debug
      .catchSome { case _ : BadCommandLine => ZIO.unit }
      .tapError( reportFinalThrowable )
      .exitCode

  val runTask =
    for
      args <- getArgs
      cfg <- ZIO.attempt(config(args.toArray))
      mbr <- work(cfg)
      _   <- reportMaybeResult(mbr)
    yield ()
end ZTMain
