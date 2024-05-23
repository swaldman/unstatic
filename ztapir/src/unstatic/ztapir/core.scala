package unstatic.ztapir

import scala.collection.*
import scala.util.Using
import sttp.tapir.files.*
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

import java.io.*
import java.net.URL
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
  ZTEndpointBinding.generic[Unit,Unit](site.siteRootedPath(fromServerRooted), ztServerEndpoint, UnitThrowableUnitLogic, NoIdentifiers)

private def publicReadOnlyUtf8HtmlEndpoint( siteRootedPath: Rooted, site : Site, task: zio.Task[String] ) : ZTServerEndpoint =
  val endpoint =
    endpointForFixedPath( site.serverRootedPath(siteRootedPath) )
      .errorOut(stringBody(CharsetUTF8))
      .out(header(Header.contentType(MediaType.TextHtml.charset(CharsetUTF8))))
      .out(htmlBodyUtf8)
  endpoint.zServerLogic( _ => errMapped(task) )

private def publicReadOnlyUtf8CssEndpoint( siteRootedPath: Rooted, site : Site, task: zio.Task[String] ) : ZTServerEndpoint =
  publicReadOnlyUtf8Endpoint( MediaType.TextCss )( siteRootedPath, site, task.map( s => immutable.ArraySeq.unsafeWrapArray(s.getBytes(CharsetUTF8))) )

private def publicReadOnlyUtf8RssEndpointFromBytes( siteRootedPath: Rooted, site : Site, task: zio.Task[immutable.ArraySeq[Byte]] ) : ZTServerEndpoint =
  publicReadOnlyUtf8Endpoint( MediaTypeRss )( siteRootedPath, site, task )

private def classLoaderResourceEndpoint( siteRootedPath : Rooted, site : Site, cl : ClassLoader, clPath : String ) : ZTServerEndpoint =
  val inputs =
    val serverRootedPath = site.serverRootedPath(siteRootedPath)
    inputsForFixedPath( serverRootedPath )
  staticResourceGetServerEndpoint[Task](inputs)( cl, clPath )

private def arraySeqByteTask( openInputStream : () => InputStream ) : Task[immutable.ArraySeq[Byte]] =
  ZIO.attempt {
    val baos = new ByteArrayOutputStream()
    Using.resource(new BufferedInputStream(openInputStream())) { bis =>
      var b : Int = bis.read()
      while (b >= 0) {
        baos.write(b)
        b = bis.read()
      }
    }
    immutable.ArraySeq.unsafeWrapArray(baos.toByteArray)
  }

private def imageProxyingMediaTypeServerEndpointAndTask( siteRootedPath: Rooted, site : Site, url : URL ) : ( MediaType, ZTServerEndpoint, Task[immutable.ArraySeq[Byte]] ) =
  val mediaType = {
    val urlStr = url.toString()
    if urlStr.endsWith(".jpg") || urlStr.endsWith(".jpeg") then
      MediaType.ImageJpeg
    else if urlStr.endsWith(".png") then
      MediaType.ImagePng
    else if urlStr.endsWith(".gif") then
      MediaType.ImageGif
    else if urlStr.endsWith(".tif") || urlStr.endsWith(".tiff") then
      MediaType.ImageTiff
    else
      throw new CantGuessImageType(s"'${urlStr}' does not have a suffix of a supported media type.")
  }
  val endpoint =
    endpointForFixedPath( site.serverRootedPath(siteRootedPath) )
      .errorOut(stringBody(CharsetUTF8))
      .out(header(Header.contentType(mediaType)))
      .out(byteArrayBody)
  val task = arraySeqByteTask(() => url.openStream)
  ( mediaType, endpoint.zServerLogic( _ => errMapped(task.map(_.toArray)) ), task )

private def publicReadOnlyUtf8Endpoint( mediaType : MediaType )( siteRootedPath: Rooted, site : Site, task: zio.Task[immutable.ArraySeq[Byte]] ) : ZTServerEndpoint =
  val endpoint =
    endpointForFixedPath( site.serverRootedPath(siteRootedPath) )
      .errorOut(stringBody(CharsetUTF8))
      .out(header(Header.contentType(mediaType)))
      .out(byteArrayBody)
  endpoint.zServerLogic( _ => errMapped(task.map(_.toArray)) )

private def staticDirectoryServingEndpoint(siteRootedPath: Rooted, site: Site, dir: JPath): ZTServerEndpoint =
  val serverRootedPath = site.serverRootedPath(siteRootedPath)
  // see https://tapir.softwaremill.com/en/latest/endpoint/static.html
  val inputs = if serverRootedPath.isRoot then emptyInput else inputsForFixedPath(serverRootedPath)
  staticFilesGetServerEndpoint[Task](inputs)(dir.toAbsolutePath.toString)

private def staticFileServingEndpoint(siteRootedPath: Rooted, site: Site, file: JPath): ZTServerEndpoint =
  val serverRootedPath = site.serverRootedPath(siteRootedPath)
  if siteRootedPath.isDir then
    if siteRootedPath.isRoot || serverRootedPath.isRoot then // second case should be impossible without first
      throw new MustRepresentDirectory(s"Illegal endpoint, the root directory must represent a directory, not a single file.")
    else
      scribe.warn(s"A UrlPath marked as a directory (would print with terminal slash) is given as endpoint for single file '${file}'.")
  val inputs = inputsForFixedPath(serverRootedPath) // we know it's not root
  staticFileGetServerEndpoint[Task](inputs)(file.toAbsolutePath.toString)

private def mediaTypeFromMimeType( mimeType : String ) : MediaType =
  MediaType.parse(mimeType) match
    case Left( message ) => throw new BadMediaType( message )
    case Right( mt )     => mt
