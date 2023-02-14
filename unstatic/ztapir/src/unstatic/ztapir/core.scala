package unstatic.ztapir

import scala.collection.*
import sttp.tapir.ztapir.*
import sttp.tapir.{Endpoint, EndpointIO, EndpointInput, EndpointOutput}
import sttp.tapir.internal.RichEndpoint
import sttp.model.{Header, MediaType, Method, StatusCode}
import sttp.tapir.EndpointOutput.MappedPair
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.ziohttp.ZioHttpToResponseBody
import unstatic.*
import unstatic.UrlPath.*
import zio.*

import java.nio.file.Path as JPath

type ZTServerEndpoint = ZServerEndpoint[Any,Any] //ServerEndpoint[Any,[t] =>> ZIO[Any,String,t]]

val NoIdentifiers = immutable.Set.empty[String]

// valid operations returning ZServerEndpoint[Nothing,Any] seems to be a Scala 3/tapir
// type inference glitch.
//
// see https://github.com/softwaremill/tapir/issues/2694
//
// for now we workaround.

extension ( badInference : ZServerEndpoint[Nothing,Any] )
  def glitchWiden : ZTServerEndpoint =
    badInference.asInstanceOf[ZTServerEndpoint]

private def endpointForFixedPath( serverRootedPath : Rooted ) : Endpoint[Unit, Unit, Unit, Unit, Any] =
  if (serverRootedPath == Rooted.root) then
    endpoint.get.in("")
  else
    serverRootedPath.elements.foldLeft(endpoint.get)( (accum, next) => accum.in( next ) )

private def inputsForFixedPath( serverRootedPath : Rooted ) : EndpointInput[Unit] =
  if (serverRootedPath == Rooted.root) then
    "" : EndpointInput[Unit] // root always represents a directory
  else
    serverRootedPath.elements.tail.foldLeft(serverRootedPath.elements.head : EndpointInput[Unit])( (accum, next) => accum / next )

private def errMapped[T]( task : Task[T] ) : zio.ZIO[Any,String,T] =
  // XXX: Should I do something to break harder on non-nonFatal errors?
  task.mapError { t =>
    import java.io.*
    val sw = new StringWriter()
    t.printStackTrace(new PrintWriter(sw))
    sw.toString()
  }

private def errMapped[S,T]( f : Function1[S,Task[T]] ) : Function1[S,zio.ZIO[Any,String,T]] =
  // XXX: Should I do something to break harder on non-nonFatal errors?
  f.andThen( errMapped )

private def redirectEndpoint( fromServerRooted : Rooted, toServerRooted : Rooted ) : Endpoint[Unit, Unit, Unit, Unit, Any] =
  endpointForFixedPath(fromServerRooted)
    .out( redirectOutputs(toServerRooted))

private def redirectOutputs( redirectToServerRooted : Rooted ) =
  statusCode(StatusCode.MovedPermanently) and header(Header.location(redirectToServerRooted.toString()))

private val UnitTask = ZIO.attempt( () )

private val CharsetUTF8 = scala.io.Codec.UTF8.charSet
private val SomeUTF8    = Some(CharsetUTF8)

val UnitThrowableUnitLogic = (_ : Unit) => (ZIO.unit : Task[Unit])
val UnitUnitUnitLogic      = UnitThrowableUnitLogic.andThen(_.mapError(_ => ()) )

val MediaTypeRss = MediaType("application","rss+xml",None,immutable.Map.empty[String,String])

private def redirectZTEndpointBinding( fromServerRooted : Rooted, toServerRooted : Rooted, site : Site ) : ZTEndpointBinding =
  val endpoint = redirectEndpoint(fromServerRooted,toServerRooted)
  val ztServerEndpoint = endpoint.zServerLogic( UnitUnitUnitLogic ).glitchWiden
  ZTEndpointBinding.Generic[Unit,Unit](site.siteRootedPath(fromServerRooted), ztServerEndpoint, UnitThrowableUnitLogic, NoIdentifiers)

private def publicReadOnlyUtf8HtmlEndpoint( siteRootedPath: Rooted, site : Site, task: zio.Task[String] ) : ZTServerEndpoint =
  val endpoint =
    endpointForFixedPath( site.serverRootedPath(siteRootedPath) )
      .errorOut(stringBody(CharsetUTF8))
      .out(header(Header.contentType(MediaType.TextHtml.charset(CharsetUTF8))))
      .out(htmlBodyUtf8)
  endpoint.zServerLogic( _ => errMapped(task) )

// XXX: should I modify this to output immutable.ArraySeq[Byte]?
private def publicReadOnlyUtf8RssEndpoint( siteRootedPath: Rooted, site : Site, task: zio.Task[immutable.ArraySeq[Byte]] ) : ZTServerEndpoint =
  val endpoint =
    endpointForFixedPath( site.serverRootedPath(siteRootedPath) )
      .errorOut(stringBody(CharsetUTF8))
      .out(header(Header.contentType(MediaTypeRss)))
      .out(byteArrayBody)
  endpoint.zServerLogic( _ => errMapped(task.map(_.toArray)) )

private def staticDirectoryServingEndpoint(siteRootedPath: Rooted, site: Site, dir: JPath): ZTServerEndpoint =
  val serverRootedPath = site.serverRootedPath(siteRootedPath)
  // see https://tapir.softwaremill.com/en/latest/endpoint/static.html
  val inputs = if serverRootedPath.isRoot then emptyInput else inputsForFixedPath(serverRootedPath)
  filesGetServerEndpoint[Task](inputs)(dir.toAbsolutePath.toString)
