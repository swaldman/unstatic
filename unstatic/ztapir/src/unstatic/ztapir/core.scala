package unstatic.ztapir

import scala.collection.*
import sttp.tapir.ztapir.*
import sttp.tapir.{Endpoint, EndpointIO, EndpointInput}
import sttp.tapir.internal.RichEndpoint
import sttp.model.{Header, MediaType, Method, StatusCode}
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.ziohttp.ZioHttpToResponseBody
import unstatic.*
import unstatic.UrlPath.*
import zio.*

import java.nio.file.Path as JPath

type ZTServerEndpoint = ZServerEndpoint[Any,Any] //ServerEndpoint[Any,[t] =>> ZIO[Any,String,t]]

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

private val DirIndexRedirectStart = "DIRECTORY INDEX REDIRECT"
private def directoryRedirectBody( fromServerRooted : Rooted ) : String =
    s"${DirIndexRedirectStart}: ${fromServerRooted.asNotDir} -> ${fromServerRooted.asDir}"

// This is a work in progress, as so far, we can't find a means of discriminating between
// paths like /mypath and /mypath/ (and tapir seems to autoredirect to the former).
//
// We see from .in(extractFromRequest( _.showShort )) only strings like GET /mypath
private def redirectOrServerDirectoryIndexZTEndpointBinding( fromServerRooted : Rooted, site : Site ) : ZTEndpointBinding =
  val endpoint =
    endpointForFixedPath(fromServerRooted)
    .in(extractFromRequest( _.showShort ))
    .errorOut(stringBody)
    .out(
      oneOf[String] (
        oneOfVariantValueMatcher(StatusCode.MovedPermanently, stringBody) {
          case (str : String) => str.startsWith(DirIndexRedirectStart)
          case other => { println("Unexpected input to redirect variant matcher: " + other); false }
        },
        oneOfVariantValueMatcher(StatusCode.Ok, htmlBodyUtf8) {
          case (str : String) => !str.startsWith(DirIndexRedirectStart)
          case other => { println("Unexpected input to serve director index variant matcher: " + other); false }
        }
    )
  )
  val logic : String => Task[String] = (s : String) => ZIO.attempt(s)
  val ztServerEndpoint = endpoint.zServerLogic(errMapped[String,String](logic)).asInstanceOf[ZTServerEndpoint] // weird type Scala 3 tapir type inference glitch
  ZTEndpointBinding(site.siteRootedPath(fromServerRooted), ztServerEndpoint, Some(ZTLogic.Generic(logic)))

private def redirectEndpoint( fromServerRooted : Rooted, toServerRooted : Rooted ) : Endpoint[Unit, Unit, Unit, Unit, Any] =
  endpointForFixedPath(fromServerRooted)
    .out(statusCode(StatusCode.MovedPermanently))
    .out(header(Header.location(toServerRooted.toString())))

private val UnitTask = ZIO.attempt( () )

private def redirectZTEndpointBinding( fromServerRooted : Rooted, toServerRooted : Rooted, site : Site ) : ZTEndpointBinding =
  val endpoint = redirectEndpoint(fromServerRooted,toServerRooted)
  val logic : Unit => ZIO[Any,Throwable,Unit] =  _ => UnitTask
  val ztServerEndpoint = endpoint.zServerLogic( logic.andThen(_.mapError(_ => ()) ) ).asInstanceOf[ZTServerEndpoint] // weird type Scala 3 tapir type inference glitch
  ZTEndpointBinding(site.siteRootedPath(fromServerRooted), ztServerEndpoint, Some(ZTLogic.Generic(logic)))

private def staticallyGenerableZTEndpointBindingWithNewSiteRootedPath( newSiteRootedPath : Rooted, site : Site, generableBinding : ZTEndpointBinding ) : ZTEndpointBinding =
  val newServerRootedPath = site.serverRootedPath(newSiteRootedPath)
  staticallyGenerableZTEndpointBinding( newSiteRootedPath, newServerRootedPath, generableBinding )

private def staticallyGenerableZTEndpointBindingWithNewServerRootedPath( newServerRootedPath : Rooted, site : Site, generableBinding : ZTEndpointBinding ) : ZTEndpointBinding =
  val newSiteRootedPath = site.siteRootedPath(newServerRootedPath)
  staticallyGenerableZTEndpointBinding( newSiteRootedPath, newServerRootedPath, generableBinding )

// Careful! nothing enforces consistency of newServer and newSite rooted paths in this method!
// Better to use one of the variants above!
private def staticallyGenerableZTEndpointBinding( newSiteRootedPath : Rooted, newServerRootedPath : Rooted, generableBinding : ZTEndpointBinding ) : ZTEndpointBinding =
  if (!generableBinding.isGenerable) then
    throw new NotStaticallyGenerable( s"ZTEndpointBinding ${generableBinding} is not statically generable, cannot be repurposed to generate the same document at a new fixed path." )
  else
    val newZTServerEndpoint = // XXX: Should we copy rather than attempt to reconstruct errorOut
      val newEndpoint = endpointForFixedPath( newServerRootedPath ).errorOut(stringBody).copy(output=generableBinding.ztServerEndpoint.output)
      val newLogicTask = (generableBinding.mbStringGenerator orElse generableBinding.mbBytesGenerator).get // since we're generable one of these must be nonEmpty
      // since we're using both the output and type of generableBinding, we know they should be consistent
      newEndpoint.zServerLogic( _ => errMapped(newLogicTask.asInstanceOf[zio.Task[generableBinding.ztServerEndpoint.OUTPUT]]))
    generableBinding.copy(siteRootedPath=newSiteRootedPath, ztServerEndpoint=newZTServerEndpoint.asInstanceOf[ZTServerEndpoint])

private def publicReadOnlyHtmlEndpoint( siteRootedPath: Rooted, site : Site, task: zio.Task[String] ) : ZTServerEndpoint =
  val endpoint =
    endpointForFixedPath( site.serverRootedPath(siteRootedPath) )
      .errorOut(stringBody)
      .out(header(Header.contentType(MediaType.TextHtml)))
      .out(stringBody)
  endpoint.zServerLogic( _ => errMapped(task) )

// XXX: should I modify this to output immutable.ArraySeq[Byte]?
private def publicReadOnlyRssEndpoint( siteRootedPath: Rooted, site : Site, task: zio.Task[String] ) : ZTServerEndpoint =
  val endpoint =
    endpointForFixedPath( site.serverRootedPath(siteRootedPath) )
      .errorOut(stringBody)
      .out(header(Header.contentType(MediaType("application","rss+xml"))))
      .out(stringBody)
  endpoint.zServerLogic( _ => errMapped(task) )

private def staticDirectoryServingEndpoint(siteRootedPath: Rooted, site: Site, dir: JPath): ZTServerEndpoint =
  val serverRootedPath = site.serverRootedPath(siteRootedPath)
  filesGetServerEndpoint[Task](inputsForFixedPath(serverRootedPath))(dir.toAbsolutePath.toString)

/**
 *  This path is server rooted, not site rooted!
 */
private def endpointStaticallyGenerableFilePath( endpointBinding : ZTEndpointBinding ) : Option[Rooted] =
  if endpointBinding.isGenerable then
    endpointStaticallyGenerableFilePath(endpointBinding.ztServerEndpoint.endpoint)
  else
    None

// we wouldn't know how to generate from this, though. service is opaque, buried in the ServerInterpreter
// private def endpointStaticallyGenerableFilePath[R,F[_]]( serverEndpoint : ServerEndpoint[R,F] ) : Option[Rooted] =
//   endpointStaticallyGenerableFilePath(serverEndpoint.endpoint)

/**
 *  This path is server rooted, not site rooted!
 */
private def endpointStaticallyGenerableFilePath[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R]( endpoint : Endpoint[SECURITY_INPUT, INPUT, ERROR_OUTPUT, OUTPUT, R] ) : Option[Rooted] =
  val inputs = endpoint.asVectorOfBasicInputs(includeAuth = true)
  val acceptableInputs = inputs.collect {
    case a : EndpointInput.FixedPath[_]                                                              => a
    case b @ EndpointInput.FixedMethod(Method(methodName),_,_) if methodName.equalsIgnoreCase("GET") => b
    case c : EndpointIO.Empty[_]                                                                     => c
  }
  if (inputs.size != acceptableInputs.size) // we have some unacceptable inputs
    // println("Unacceptable inputs: " + inputs.filter( inp => !acceptableInputs.contains(inp) ).mkString(", "))
    None
  else
    Some( Rooted.fromElements( inputs.collect{ case input : EndpointInput.FixedPath[_] => input.s } : _* ) )
