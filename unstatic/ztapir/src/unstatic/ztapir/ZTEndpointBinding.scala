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
object ZTEndpointBinding:
  trait Source:
    def endpointBindings : immutable.Seq[ZTEndpointBinding]

  def staticDirectoryServing( siteRootedPath: Rooted, site: ZTSite, dir : JPath, identifiers : immutable.Set[String] ) : ZTEndpointBinding.FromStaticDirectory =
    FromStaticDirectory(siteRootedPath, staticDirectoryServingEndpoint( siteRootedPath, site, dir ), dir, identifiers)

  def staticDirectoryServing(siteLocation: ZTSite#SiteLocation, dir: JPath, identifiers : immutable.Set[String] ): ZTEndpointBinding.FromStaticDirectory =
    staticDirectoryServing(siteLocation.siteRootedPath, siteLocation.site, dir, identifiers)

  def staticFileServing( siteRootedPath: Rooted, site: ZTSite, file : JPath, identifiers : immutable.Set[String] ) : ZTEndpointBinding.FromStaticFile =
    FromStaticFile(siteRootedPath, staticFileServingEndpoint( siteRootedPath, site, file ), file, identifiers)

  def staticFileServing(siteLocation: ZTSite#SiteLocation, file: JPath, identifiers : immutable.Set[String] ): ZTEndpointBinding.FromStaticFile =
    staticFileServing(siteLocation.siteRootedPath, siteLocation.site, file, identifiers)

  def publicReadOnlyHtml( siteRootedPath: Rooted, site : ZTSite, task: zio.Task[String], mediaDirSiteRooted : Option[Rooted], identifiers : immutable.Set[String]  ) : ZTEndpointBinding.StringGenerable =
    StringGenerable( siteRootedPath, publicReadOnlyUtf8HtmlEndpoint( siteRootedPath, site, task ), task, mediaDirSiteRooted, MediaType.TextHtml.charset(CharsetUTF8), CharsetUTF8, identifiers )

  def publicReadOnlyHtml(siteLocation: ZTSite#SiteLocation, task: zio.Task[String], mediaDirSiteRooted : Option[Rooted], identifiers : immutable.Set[String] ): ZTEndpointBinding.StringGenerable =
    publicReadOnlyHtml(siteLocation.siteRootedPath, siteLocation.site, task, mediaDirSiteRooted, identifiers)

  def publicReadOnlyRss( siteRootedPath: Rooted, site : ZTSite, task: zio.Task[immutable.ArraySeq[Byte]], identifiers : immutable.Set[String]  ) : ZTEndpointBinding.BytesGenerable =
    BytesGenerable( siteRootedPath, publicReadOnlyUtf8RssEndpoint( siteRootedPath, site, task ), task, MediaTypeRss, identifiers )

  def publicReadOnlyRss(siteLocation: ZTSite#SiteLocation, task: zio.Task[immutable.ArraySeq[Byte]], identifiers : immutable.Set[String] ): ZTEndpointBinding.BytesGenerable =
    publicReadOnlyRss(siteLocation.siteRootedPath, siteLocation.site, task, identifiers)

  sealed trait Generable extends ZTEndpointBinding:
    val contentType    : MediaType
    val bytesGenerator : Task[immutable.ArraySeq[Byte]]

  sealed trait FromFileSystem extends ZTEndpointBinding:
    val source : JPath

  case class StringGenerable(siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, generator : Task[String], mediaDirSiteRooted : Option[Rooted], contentType : MediaType, charset : Charset, identifiers : immutable.Set[String]) extends Generable:
    val bytesGenerator = generator.map( s => immutable.ArraySeq.unsafeWrapArray(s.getBytes(charset)) )
  case class BytesGenerable( siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, generator : Task[immutable.ArraySeq[Byte]], contentType : MediaType, identifiers : immutable.Set[String] ) extends Generable:
    val bytesGenerator = generator
  case class FromStaticDirectory(siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, dir : JPath, identifiers : immutable.Set[String]) extends FromFileSystem:
    val source = dir
  case class FromStaticFile(siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, file : JPath, identifiers : immutable.Set[String]) extends FromFileSystem:
    val source = file
  case class Generic[I,O](siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, coreLogic : I => Task[O], identifiers : immutable.Set[String]) extends ZTEndpointBinding

// Keys (initial elements) are site-rooted, but endpoints are server rooted!
sealed trait ZTEndpointBinding extends AnyBinding:
  val siteRootedPath : Rooted
  val ztServerEndpoint : ZTServerEndpoint
  val identifiers : immutable.Set[String]
