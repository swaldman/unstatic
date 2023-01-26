package unstatic.ztapir

import sttp.tapir.ztapir.*
import sttp.tapir.server.interceptor.log.DefaultServerLog
import sttp.tapir.server.ziohttp.{ZioHttpInterpreter, ZioHttpServerOptions}
import zio.http.{Http, HttpApp}
import zio.http.{Server, ServerConfig}
import zio.*

import unstatic.UrlPath.*

class ZTSiteHttpServer(site : ZTSite) extends ZIOAppDefault:

  inline def options( inline verbose : Boolean ) =
    if verbose then
      // modified from https://github.com/longliveenduro/zio-geolocation-tapir-tapir-starter/blob/b79c88b9b1c44a60d7c547d04ca22f12f420d21d/src/main/scala/com/tsystems/toil/Main.scala
      ZioHttpServerOptions
        .customiseInterceptors
        .serverLog(
          DefaultServerLog[Task](
            doLogWhenReceived = msg => ZIO.succeed(println(msg)),
            doLogWhenHandled = (msg, error) => ZIO.succeed(error.fold(println(msg))(err => println( s"msg: ${msg}, err: ${err}" ))),
            doLogAllDecodeFailures = (msg, error) => ZIO.succeed(error.fold(println(msg))(err => println( s"msg: ${msg}, err: ${err}" ))),
            doLogExceptions = (msg: String, exc: Throwable) => ZIO.succeed(println(s"msg: ${msg}, exc: ${exc}")),
            noLog = ZIO.unit
          )
        )
        .options
    else
      ZioHttpServerOptions.default

  val serverOptions = options(false)

  def buildApp( endpointSource: ZTEndpointBinding.Source ) : HttpApp[Any,Throwable] =
    val endpointBindings = endpointSource.endpointBindings
    val endpoints = endpointBindings.map( _.ztServerEndpoint )
    if (endpoints.isEmpty) throw new Exception("No endpoints defined.") //XXX: Better Exceptions
    def toHttp( endpoint : ZTServerEndpoint) = ZioHttpInterpreter( serverOptions ).toHttp( endpoint )
      endpoints.tail.foldLeft(toHttp(endpoints.head))( (accum, next) => accum ++ toHttp(next) )

  // starting the server
  override def run =
    val server =
      for
        app       <- ZIO.attempt(buildApp(site))
        server    <- Server.serve(app)
      yield server
    server
      .provide(ServerConfig.live(ServerConfig.default.port(8999)), Server.live).debug
      .exitCode
