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
  object Static:
    case class Config( ignorePrefixes : immutable.Seq[Rooted] )

    given defaultConfig : Config = Config( Nil )

    def generate( site : ZTSite, generateTo : JPath )( using cfg : Config ) =
      ZTStaticGen.generateZTSite( site, generateTo, cfg.ignorePrefixes )

trait ZTSite extends Site with ZTEndpointBinding.Source
