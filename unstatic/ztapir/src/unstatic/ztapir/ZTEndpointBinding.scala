package unstatic.ztapir

import java.net.URL
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

  val IdentifierOrdering = AnyBinding.IdentifierOrdering

  def staticDirectoryServing( siteRootedPath: Rooted, site: ZTSite, dir : JPath, identifiers : immutable.Set[String] ) : ZTEndpointBinding.FromStaticDirectory =
    FromStaticDirectory(siteRootedPath, staticDirectoryServingEndpoint( siteRootedPath, site, dir ), dir, immutable.SortedSet.from(identifiers)(using IdentifierOrdering))

  def staticDirectoryServing(siteLocation: ZTSite#SiteLocation, dir: JPath, identifiers : immutable.Set[String] ): ZTEndpointBinding.FromStaticDirectory =
    staticDirectoryServing(siteLocation.siteRootedPath, siteLocation.site, dir, identifiers)

  def staticFileServing( siteRootedPath: Rooted, site: ZTSite, file : JPath, identifiers : immutable.Set[String] ) : ZTEndpointBinding.FromStaticFile =
    FromStaticFile(siteRootedPath, staticFileServingEndpoint( siteRootedPath, site, file ), file, immutable.SortedSet.from(identifiers)(using IdentifierOrdering))

  def staticFileServing(siteLocation: ZTSite#SiteLocation, file: JPath, identifiers : immutable.Set[String] ): ZTEndpointBinding.FromStaticFile =
    staticFileServing(siteLocation.siteRootedPath, siteLocation.site, file, identifiers)

  def imageProxying( siteRootedPath : Rooted, site : ZTSite, url : URL, identifiers : immutable.Set[String]) : ZTEndpointBinding.BytesGenerable =
    val (mediaType, ztServerEndpoint, task ) = imageProxyingMediaTypeServerEndpointAndTask( siteRootedPath: Rooted, site : Site, url : URL )
    BytesGenerable( siteRootedPath, ztServerEndpoint, task, mediaType, immutable.SortedSet.from(identifiers)(using IdentifierOrdering) )

  def imageProxying( siteLocation :  ZTSite#SiteLocation, site : ZTSite, url : URL, identifiers : immutable.Set[String]) : ZTEndpointBinding.BytesGenerable =
    imageProxying(siteLocation.siteRootedPath, site, url, identifiers)

  def publicReadOnlyHtml( siteRootedPath: Rooted, site : ZTSite, task: zio.Task[String], mediaDirSiteRooted : Option[Rooted], identifiers : immutable.Set[String]  ) : ZTEndpointBinding.StringGenerable =
    StringGenerable( siteRootedPath, publicReadOnlyUtf8HtmlEndpoint( siteRootedPath, site, task ), task, mediaDirSiteRooted, MediaType.TextHtml.charset(CharsetUTF8), CharsetUTF8, immutable.SortedSet.from(identifiers)(using IdentifierOrdering))

  def publicReadOnlyHtml(siteLocation: ZTSite#SiteLocation, task: zio.Task[String], mediaDirSiteRooted : Option[Rooted], identifiers : immutable.Set[String] ): ZTEndpointBinding.StringGenerable =
    publicReadOnlyHtml(siteLocation.siteRootedPath, siteLocation.site, task, mediaDirSiteRooted, identifiers)

  def publicReadOnlyCss( siteRootedPath: Rooted, site : ZTSite, task: zio.Task[String], mediaDirSiteRooted : Option[Rooted], identifiers : immutable.Set[String]  ) : ZTEndpointBinding.StringGenerable =
    StringGenerable( siteRootedPath, publicReadOnlyUtf8CssEndpoint( siteRootedPath, site, task ), task, mediaDirSiteRooted, MediaType.TextHtml.charset(CharsetUTF8), CharsetUTF8, immutable.SortedSet.from(identifiers)(using IdentifierOrdering))

  def publicReadOnlyCss(siteLocation: ZTSite#SiteLocation, task: zio.Task[String], mediaDirSiteRooted : Option[Rooted], identifiers : immutable.Set[String] ): ZTEndpointBinding.StringGenerable =
    publicReadOnlyCss(siteLocation.siteRootedPath, siteLocation.site, task, mediaDirSiteRooted, identifiers)

  def publicReadOnlyRss( siteRootedPath: Rooted, site : ZTSite, task: zio.Task[immutable.ArraySeq[Byte]], identifiers : immutable.Set[String]  ) : ZTEndpointBinding.BytesGenerable =
    BytesGenerable( siteRootedPath, publicReadOnlyUtf8RssEndpointFromBytes( siteRootedPath, site, task ), task, MediaTypeRss, immutable.SortedSet.from(identifiers)(using IdentifierOrdering))

  def publicReadOnlyRss(siteLocation: ZTSite#SiteLocation, task: zio.Task[immutable.ArraySeq[Byte]], identifiers : immutable.Set[String] ): ZTEndpointBinding.BytesGenerable =
    publicReadOnlyRss(siteLocation.siteRootedPath, siteLocation.site, task, identifiers)

  def generic[I,O](siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, coreLogic : I => Task[O], identifiers : immutable.Set[String]) : ZTEndpointBinding.Generic[I,O] =
    Generic(siteRootedPath, ztServerEndpoint, coreLogic, immutable.SortedSet.from(identifiers)(using IdentifierOrdering))

  sealed trait Generable extends ZTEndpointBinding:
    val contentType    : MediaType
    val bytesGenerator : Task[immutable.ArraySeq[Byte]]

  sealed trait FromFileSystem extends ZTEndpointBinding:
    val source : JPath

  case class StringGenerable private[ZTEndpointBinding] (siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, generator : Task[String], mediaDirSiteRooted : Option[Rooted], contentType : MediaType, charset : Charset, identifiers : immutable.SortedSet[String]) extends Generable:
    val bytesGenerator = generator.map( s => immutable.ArraySeq.unsafeWrapArray(s.getBytes(charset)) )
  case class BytesGenerable private[ZTEndpointBinding] ( siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, generator : Task[immutable.ArraySeq[Byte]], contentType : MediaType, identifiers : immutable.SortedSet[String] ) extends Generable:
    val bytesGenerator = generator
  case class FromStaticDirectory private[ZTEndpointBinding] (siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, dir : JPath, identifiers : immutable.SortedSet[String]) extends FromFileSystem:
    val source = dir
  case class FromStaticFile private[ZTEndpointBinding] (siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, file : JPath, identifiers : immutable.SortedSet[String]) extends FromFileSystem:
    val source = file
  case class Generic[I,O] private[ZTEndpointBinding] (siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, coreLogic : I => Task[O], identifiers : immutable.SortedSet[String]) extends ZTEndpointBinding

// Keys (initial elements) are site-rooted, but endpoints are server rooted!
sealed trait ZTEndpointBinding extends AnyBinding:
  val siteRootedPath : Rooted
  val ztServerEndpoint : ZTServerEndpoint
  val identifiers : immutable.SortedSet[String]
