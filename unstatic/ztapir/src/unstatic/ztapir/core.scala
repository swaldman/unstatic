package unstatic.ztapir

import scala.collection.*
import sttp.tapir.ztapir.*
import sttp.tapir.{Endpoint, EndpointIO, EndpointInput}
import sttp.tapir.internal.RichEndpoint
import sttp.model.{Header, MediaType, Method}
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.ziohttp.ZioHttpToResponseBody
import unstatic.*
import unstatic.UrlPath.*
import zio.*

import java.nio.file.Path as JPath

trait ZTSite extends Site with ZTEndpointBinding.Source

trait ZTStaticResources[S <: ZTSite] extends StaticResources[S] with ZTEndpointBinding.Source with StaticLocationBinding.Source:
  // Keys are site-rooted, but endpoints are server rooted!
  def endpointBindings: immutable.Seq[ZTEndpointBinding] =
    locationBindings.map { case StaticLocationBinding(siteRootedPath, source) => ZTEndpointBinding.staticDirectoryServing( siteRootedPath, site, source ) }

type ZTServerEndpoint = ZServerEndpoint[Any,Any] //ServerEndpoint[Any,[t] =>> ZIO[Any,String,t]]

private def endpointForFixedPath( serverRootedPath : Rooted ) : Endpoint[Unit, Unit, Unit, Unit, Any] =
  if (serverRootedPath == Rooted.root) then
    endpoint.get.in("")
  else
    serverRootedPath.elements.foldLeft(endpoint.get)( (accum, next) => accum.in( next ) )

private def inputsForFixedPath( serverRootedPath : Rooted ) : EndpointInput[Unit] =
  if (serverRootedPath == Rooted.root) then
    "" : EndpointInput[Unit]
  else
    serverRootedPath.elements.tail.foldLeft(serverRootedPath.elements.head : EndpointInput[Unit])( (accum, next) => accum / next )

private def publicReadOnlyHtmlEndpoint( siteRootedPath: Rooted, site : Site, task: zio.Task[String] ) : ZTServerEndpoint =
  // XXX: Should I do something to break harder on non-nonFatal errors?
  val errMappedTask = task.mapError { t =>
    import java.io.*
    val sw = new StringWriter()
    t.printStackTrace(new PrintWriter(sw))
    sw.toString()
  }
  val endpoint =
    endpointForFixedPath(siteRootedPath.reroot(site.basePath))
      .errorOut(stringBody)
      .out(header(Header.contentType(MediaType.TextHtml)))
      .out(stringBody)
  endpoint.zServerLogic( _ => errMappedTask )

private def endpointStaticallyGenerableFilePath[R,F[_]]( serverEndpoint : ServerEndpoint[R,F] ) : Option[Rooted] =
  endpointStaticallyGenerableFilePath(serverEndpoint.endpoint)

private def staticDirectoryServingEndpoint(siteRootedPath: Rooted, site: Site, dir: JPath): ZTServerEndpoint =
  val serverRootedPath = site.serverRootedPath(siteRootedPath)
  filesGetServerEndpoint[Task](inputsForFixedPath(serverRootedPath))(dir.toAbsolutePath.toString)


private def endpointStaticallyGenerableFilePath[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]( endpoint : Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R] ) : Option[Rooted] =
  val inputs = endpoint.asVectorOfBasicInputs(includeAuth = true)
  val acceptableInputs = inputs.collect {
    case a : EndpointInput.FixedPath[_]                                                                           => a
    case b @ EndpointInput.FixedMethod(Method(methodName),_,_) if methodName.equalsIgnoreCase("GET") => b
    case c : EndpointIO.Empty[_]                                                                                  => c
  }
  if (inputs.size != acceptableInputs.size) // we have some unacceptable inputs
    // println("Unacceptable inputs: " + inputs.filter( inp => !acceptableInputs.contains(inp) ).mkString(", "))
    None
  else
    Some( Rooted.fromElements( inputs.collect{ case input : EndpointInput.FixedPath[_] => input.s } : _* ) )
