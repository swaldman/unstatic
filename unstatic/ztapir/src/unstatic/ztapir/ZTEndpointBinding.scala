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

case class ZTEndpointBinding( siteRootedPath : Rooted, ztServerEndpoint : ZTServerEndpoint, mbLogic : Option[ZTLogic[_,_]] ):
  lazy val mbGenerator : Option[Task[String]] =
    endpointStaticallyGenerableFilePath(ztServerEndpoint).flatMap { _ =>
      mbLogic match
        case Some(us : ZTLogic.UnitString) => Some(us.task)
        case _                             => None
    }
