package unstatic.ztapir

import scala.collection.*
import java.nio.file.Path as JPath

import unstatic.*
import unstatic.UrlPath.*

import zio.*

/**
 *  Endpoints are statically-generable iff their endpoint is and
 *  their logic is available as Unit => Task[String] (for now)
 *
 *  Keys (initial elements) are site-rooted, but endpoints are server rooted!
 */
object ZTEndpointBinding:
  // Keys (initial elements) are site-rooted, but endpoints are server rooted!
  trait Source:
    def endpointBindings : immutable.Seq[ZTEndpointBinding]

  def staticDirectoryServing( siteRootedPath: Rooted, site: ZTSite, dir : JPath ) : ZTEndpointBinding =
    ZTEndpointBinding(siteRootedPath, staticDirectoryServingEndpoint( siteRootedPath, site, dir ), None)

  def staticDirectoryServing(siteLocation: ZTSite#SiteLocation, dir: JPath): ZTEndpointBinding =
    staticDirectoryServing(siteLocation.siteRootedPath, siteLocation.site, dir)

  def publicReadOnlyHtml( siteRootedPath: Rooted, site : ZTSite, task: zio.Task[String] ) : ZTEndpointBinding =
    ZTEndpointBinding( siteRootedPath, publicReadOnlyHtmlEndpoint( siteRootedPath, site, task ), Some(ZTLogic.UnitString( task )) )

  def publicReadOnlyHtml(siteLocation: ZTSite#SiteLocation, task: zio.Task[String]): ZTEndpointBinding =
    publicReadOnlyHtml(siteLocation.siteRootedPath, siteLocation.site, task)

  def publicReadOnlyRss( siteRootedPath: Rooted, site : ZTSite, task: zio.Task[String] ) : ZTEndpointBinding =
    ZTEndpointBinding( siteRootedPath, publicReadOnlyRssEndpoint( siteRootedPath, site, task ), Some(ZTLogic.UnitString( task )) )

  def publicReadOnlyRss(siteLocation: ZTSite#SiteLocation, task: zio.Task[String]): ZTEndpointBinding =
    publicReadOnlyRss(siteLocation.siteRootedPath, siteLocation.site, task)


case class ZTEndpointBinding( siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, mbLogic : Option[ZTLogic[_,_]] ):

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