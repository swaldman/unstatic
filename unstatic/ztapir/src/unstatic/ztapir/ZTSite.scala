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
      val Default = Dynamic(DefaultPort, DefaltServerInterpreterOptions)
      given Config.Dynamic = Default
    case class Dynamic( port: Int, serverInterpreterOptions: ZioHttpServerOptions[Any] )
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
      val endpoints = endpointBindings.map(_.ztServerEndpoint)
      if (endpoints.isEmpty) throw new Exception("No endpoints defined.") //XXX: Better Exceptions

      def toHttp(endpoint: ZTServerEndpoint): Http[Any, Throwable, Request, Response] = ZioHttpInterpreter(cfg.serverInterpreterOptions).toHttp(endpoint)

      endpoints.tail.foldLeft(toHttp(endpoints.head))((accum, next) => accum ++ toHttp(next))

    val configLayer = ServerConfig.live(ServerConfig.default.port(cfg.port))
    val server =
      for
        app <- ZIO.attempt(buildApp(site))
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
              opt[Boolean]("verbose")
                .action( (x, cfg) => cfg.copy( cfgDynamic = cfg.cfgDynamic.copy(serverInterpreterOptions = Config.Dynamic.interpreterOptions(x)) ) )
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
              opt[Boolean]("verbose")
                .action( (x, cfg) => cfg.copy( cfgDynamic = cfg.cfgDynamic.copy(serverInterpreterOptions = Config.Dynamic.interpreterOptions(x)) ) )
            )
       )
      OParser.parse(parser1, args, Config()) match
        case Some(cfg) => cfg
        case _ => throw new BadCommandLine("Bad command line options provided: " + args.mkString(" "))

    def work( cfg : Config ) : ZIO[Any, Throwable, ZTStaticGen.Result] | ZIO[Any, Throwable, Unit] =
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

    override def run =
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
