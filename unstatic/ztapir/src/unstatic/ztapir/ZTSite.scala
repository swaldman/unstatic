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

import org.jsoup.Jsoup
import org.jsoup.nodes.Element

object ZTSite:
  object Config:
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
        endpointBindings.map { origBinding =>
          origBinding match
            case gen : ZTEndpointBinding.Generable =>
              directoryIndexDirectoryBindingByIndexBinding.get(gen) match
                case Some( redirectBindings ) => redirectBindings :+ gen
                case None                     => Seq( gen )
            case _ => Seq( origBinding )
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

    override def run =
      runTask
        .catchSome { case _ : BadCommandLine => ZIO.unit }
        .tapError( t => ZIO.attempt( t.printStackTrace() ) )
        .exitCode

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

trait ZTSite extends Site with ZTEndpointBinding.Source:
  override def allBindings : immutable.Seq[AnyBinding] = this.endpointBindings ++ super.allBindings

  lazy val siteRootedPathByIdentifier =
    allBindings.reverse.flatMap( b => b.identifiers.toSeq.map(id => (id, b.siteRootedPath)) ).toMap

  def publicReadOnlyHtml(siteLocation: SiteLocation, task: zio.Task[String], identifiers : immutable.Set[String], resolveHashSpecials : Boolean, memoize : Boolean ) : ZTEndpointBinding =
    def resolvingTask : zio.Task[String] =
      val sourceSiteRooted = siteLocation.siteRootedPath
      task.map( rawHtml => htmlResolveHashSpecials(sourceSiteRooted.toString(), siteLocation.siteRootedPath, rawHtml ) )
    val base        : zio.Task[String] = if resolveHashSpecials then resolvingTask else task
    val mbMemoizing : zio.Task[String] = if memoize then base.memoize.flatten else base
    ZTEndpointBinding.publicReadOnlyHtml(siteLocation, mbMemoizing, identifiers )

  private def htmlResolveHashSpecials( sourceId : String, sourceSiteRooted : Rooted, unresolvedHtml : String ) : String =
    val jsoupDoc = org.jsoup.Jsoup.parse(unresolvedHtml)
    mutateHtmlResolveHashSpecials( jsoupDoc, sourceId, sourceSiteRooted, None, true )
    jsoupDoc.outerHtml()

  def mutateHtmlResolveHashSpecials( parentElem : Element, sourceId : String, sourceSiteRooted : Rooted, mbMediaDirSiteRooted : Option[Rooted], atTopLevel : Boolean ) : Unit =
    def mutateReplace(cssQuery : String, refAttr : String) : Unit =
      import scala.jdk.CollectionConverters._
      parentElem.select(cssQuery).asScala.foreach { elem =>
        val rawHref = elem.attr(refAttr)
        val shinyHref = replaceMaybeHashSpecial(sourceId, sourceSiteRooted, rawHref, mbMediaDirSiteRooted, atTopLevel)
        elem.attr(refAttr, shinyHref)
      }

    mutateReplace("a","href")
    mutateReplace("img","src")
    mutateReplace("link","href")

  private def replaceMaybeHashSpecial( sourceId : String, sourceSiteRooted : Rooted, href : String, mbMediaDirSiteRooted : Option[Rooted], atTopLevel : Boolean ) : String =
    if href(0) == '#' then
      if href.startsWith("##") then
        val id = href.drop(2)
        siteRootedPathByIdentifier.get(id) match
          case Some(path) => sourceSiteRooted.relativizeSibling(path).toString()
          case None =>
            scribe.warn(s"${sourceId}: Special hash reference '${href}' could not be resolved to an identifier, left as-is.")
            href
      else if href.startsWith("#/") then
        val destSiteRooted = Rooted(href.drop(1))
        sourceSiteRooted.relativize(destSiteRooted).toString()
      else if href.startsWith("#./") then
        mbMediaDirSiteRooted match
          case Some( mediaDirSiteRooted ) =>
            val relpath = Rel(href.drop(3))
            val destSiteRooted = mediaDirSiteRooted.resolve(relpath)
            sourceSiteRooted.relativize(destSiteRooted).toString()
          case None =>
            scribe.warn(s"${sourceId}: Special hash reference '${href}' is relative to a mediaDir, but no mediaDir is available in this context. Left as-is.")
            href
      else if href.startsWith("""#\""") then
        if atTopLevel then // we only unescape at the top level, otherwise we might accidentally escape to something later resolved
          "#" + href.drop(2) // lose one backslash
        else
          href
      else
        href
    else
      href
