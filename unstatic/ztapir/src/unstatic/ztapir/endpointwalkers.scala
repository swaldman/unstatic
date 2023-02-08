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

import EndpointOutput.{Pair, MappedPair}

// we don't use any of this anymore, we encode what we care about in ZTEndpointBinding types and args,
// rather than walking endpoint datastructures to find what we care about.

// i'm keeping this around just for reference, if we ever want to walk endpoint datastructures in future

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

def walkOutput[T]( output : EndpointOutput[?], f : Function1[EndpointOutput[?],Function1[T,T]], accum : T ) : T =
  val newAccum = f(output)(accum)
  output match
    case p : Pair[?,?,?] =>
      val afterLeft = walkOutput( p.left, f, newAccum )
      walkOutput( p.right, f, afterLeft )
    case mp : MappedPair[_,_,_,_] =>
      walkOutput( mp.output, f, newAccum )
    case _ => newAccum

def outputsAsVector( output : EndpointOutput[?] ) : Vector[EndpointOutput[?]] =
  val f = (eo : EndpointOutput[?]) => (v : Vector[EndpointOutput[?]] ) => v :+ eo
  walkOutput(output, f, Vector.empty[EndpointOutput[?]])


