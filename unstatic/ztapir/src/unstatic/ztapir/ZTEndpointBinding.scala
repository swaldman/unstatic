package unstatic.ztapir

import scala.collection.*
import java.nio.file.Path as JPath
import java.nio.charset.Charset

import unstatic.*
import unstatic.UrlPath.*

import sttp.model.MediaType

import zio.*

/**
 *  Endpoints are statically-generable iff their endpoint is and
 *  their logic is available as Unit => Task[String] (for now)
 *
 *  Keys (initial elements) are site-rooted, but endpoints are server rooted!
 */
// TODO: Reorganize this into a type hierarchy. They've grown too complex.
object ZTEndpointBinding:
  trait Source:
    def endpointBindings : immutable.Seq[ZTEndpointBinding]

  def staticDirectoryServing( siteRootedPath: Rooted, site: ZTSite, dir : JPath ) : ZTEndpointBinding =
    FromStaticDirectory(siteRootedPath, staticDirectoryServingEndpoint( siteRootedPath, site, dir ), dir)

  def staticDirectoryServing(siteLocation: ZTSite#SiteLocation, dir: JPath): ZTEndpointBinding =
    staticDirectoryServing(siteLocation.siteRootedPath, siteLocation.site, dir)

  def publicReadOnlyHtml( siteRootedPath: Rooted, site : ZTSite, task: zio.Task[String] ) : ZTEndpointBinding =
    StringGenerable( siteRootedPath, publicReadOnlyUtf8HtmlEndpoint( siteRootedPath, site, task ), task, MediaType.TextHtml.charset(CharsetUTF8), CharsetUTF8 )

  def publicReadOnlyHtml(siteLocation: ZTSite#SiteLocation, task: zio.Task[String]): ZTEndpointBinding =
    publicReadOnlyHtml(siteLocation.siteRootedPath, siteLocation.site, task)

  def publicReadOnlyRss( siteRootedPath: Rooted, site : ZTSite, task: zio.Task[immutable.ArraySeq[Byte]] ) : ZTEndpointBinding =
    BytesGenerable( siteRootedPath, publicReadOnlyUtf8RssEndpoint( siteRootedPath, site, task ), task, MediaTypeRss )

  def publicReadOnlyRss(siteLocation: ZTSite#SiteLocation, task: zio.Task[immutable.ArraySeq[Byte]]): ZTEndpointBinding =
    publicReadOnlyRss(siteLocation.siteRootedPath, siteLocation.site, task)

  trait Generable extends ZTEndpointBinding:
    val contentType    : MediaType
    val bytesGenerator : Task[immutable.ArraySeq[Byte]]

  case class StringGenerable(siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, generator : Task[String], contentType : MediaType, charset : Charset) extends Generable:
    val bytesGenerator = generator.map( s => immutable.ArraySeq.unsafeWrapArray(s.getBytes(charset)) )
  case class BytesGenerable( siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, generator : Task[immutable.ArraySeq[Byte]], contentType : MediaType ) extends Generable:
    val bytesGenerator = generator
  case class FromStaticDirectory(siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, dir : JPath) extends ZTEndpointBinding
  case class Generic[I,O](siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, coreLogic : I => Task[O]) extends ZTEndpointBinding

// Keys (initial elements) are site-rooted, but endpoints are server rooted!
sealed trait ZTEndpointBinding:
  val siteRootedPath : Rooted
  val ztServerEndpoint : ZTServerEndpoint

//, mbLogic : Option[ZTLogic[_,_]], mbCharset : Option[Charset], fromStaticLocation : Boolean ):
/*
  /**
   * Be sure to use this if you want control of the charset, otherwise you'll be stuck with UTF8 bytes
   */
  lazy val mbStringGenerator : Option[Task[String]] =
    endpointStaticallyGenerableFilePath(ztServerEndpoint.endpoint).flatMap { _ =>
      mbLogic match
        case Some(us : ZTLogic.UnitString) => Some(us.task)
        case _                             => None
    }

  lazy val mbBytesGenerator : Option[Task[immutable.ArraySeq[Byte]]] =
    endpointStaticallyGenerableFilePath(ztServerEndpoint.endpoint).flatMap { _ =>
      mbLogic match
        case Some(us : ZTLogic.UnitString)       => Some(us.task.map(s => immutable.ArraySeq.from(s.getBytes(scala.io.Codec.UTF8.charSet))))
        case Some(us : ZTLogic.UnitArraySeqByte) => Some(us.task)
        case _                                   => None
    }

  def isStringGenerable : Boolean = mbStringGenerator.nonEmpty
  def isBytesGenerable  : Boolean = mbBytesGenerator.nonEmpty
  def isGenerable       : Boolean = isBytesGenerable
*/