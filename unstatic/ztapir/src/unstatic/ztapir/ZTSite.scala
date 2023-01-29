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

    val DefaultPort = 8999

    case class Config(port: Int, serverInterpreterOptions: ZioHttpServerOptions[Any])

    given defaultConfig: Config = Config(DefaultPort, DefaltServerInterpreterOptions)

    def serve(site: ZTSite)(using cfg: Config) =
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

    object Main:
      case class Options( port : Int = DefaultPort, verbose : Boolean = false )
    abstract class Main(site: ZTSite, executableName : String = "serve-site") extends ZIOAppDefault:
      def options( args : Array[String] ) : Main.Options =
        import scopt.OParser
        val builder = OParser.builder[Main.Options]
        val parser1 =
          import builder._
          OParser.sequence(
            programName(executableName),
            opt[Int]('p', "port")
              .action((x, opts) => opts.copy(port = x))
              .text("the port on which to serve HTTP"),
            opt[Boolean]("verbose")
              .action((x, opts) => opts.copy(verbose = x))
          )
        OParser.parse(parser1, args, Main.Options()) match {
          case Some(opts) => opts
          case _ => throw new Exception("Bad command line options provided.") // XXX: Better Exceptions
        }
      def config( options : Main.Options ) =
        Config(
            port = options.port,
            serverInterpreterOptions = if options.verbose then VerboseServerInterpreterOptions else DefaltServerInterpreterOptions
        )
      override def run =
        for
          args    <- getArgs
          options <- ZIO.attempt( options(args.toArray) )
          _       <- ZTSite.Dynamic.serve(site)(using config(options)).debug.exitCode
        yield ()

  object Static:
    case class Config( ignorePrefixes : immutable.Seq[Rooted] )

    given defaultConfig : Config = Config( Nil )

    def generate( site : ZTSite, generateTo : JPath )(using cfg : Config) =
      ZTStaticGen.generateZTSite( site, generateTo, cfg.ignorePrefixes )

    object Main:
      case class Options( generateTo : JPath = JPath.of("public"), ignorePrefixes : Seq[Rooted] = Nil )
    abstract class Main(site: ZTSite, executableName: String = "generate-site") extends ZIOAppDefault:
      def options( args : Array[String] ) : Main.Options =
        import scopt.OParser
        val builder = OParser.builder[Main.Options]
        val parser1 =
          import builder._
          OParser.sequence(
            programName(executableName),
            opt[java.io.File]('o', "out")
              .action((x, opts) => opts.copy(generateTo = x.toPath))
              .valueName("<dir>")
              .text("the output directory, into which the site will be generated"),
            opt[Seq[String]]("ignore-prefixes")
              .action((x, opts) => opts.copy(ignorePrefixes = x.map(Rooted.parse)))
              .valueName("<path1>,<path2>,...")
          )
        OParser.parse(parser1, args, Main.Options()) match {
          case Some(opts) => opts
          case _ => throw new Exception("Bad command line options provided.") // XXX: Better Exceptions
        }
      override def run =
        for
          args    <- getArgs
          options <- ZIO.attempt( options(args.toArray) )
          _       <- ZTSite.Static.generate(site,options.generateTo)(using Config(options.ignorePrefixes.toSeq)).debug.exitCode
        yield ()

  trait Composite extends ZTSite with Site.Composite:
    def endpointBindingSources : immutable.Seq[ZTEndpointBinding.Source]
    def endpointBindings : immutable.Seq[ZTEndpointBinding] = endpointBindingSources.flatMap( _.endpointBindings )
  end Composite

trait ZTSite extends Site with ZTEndpointBinding.Source
