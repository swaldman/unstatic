package unstatic

import scala.collection.*
import unstatic.UrlPath.*

object Site:
  trait Composite extends Site:
    def staticLocationBindingSources : immutable.Seq[StaticLocationBinding.Source]

    def locationBindings : immutable.Seq[StaticLocationBinding] = staticLocationBindingSources.flatMap( _.locationBindings )
  end Composite

trait Site extends StaticLocationBinding.Source:
  def serverUrl : Abs
  def basePath  : Rooted
  def sitePath  : Abs = serverUrl.embedRoot(basePath)

  def siteRoot = sitePath

  def serverRootedPath( fromSiteRootedPath : Rooted ) : Rooted = basePath.embedRoot( fromSiteRootedPath )
  def serverRootedPath( fromSiteRootedPath : String ) : Rooted = serverRootedPath( Rooted(fromSiteRootedPath) )

  case class SiteLocation( siteRootedPath : Rooted, site : this.type ):
    def serverRootedPath = site.serverRootedPath(siteRootedPath)
    def relative(using base: PageBase) = base.siteRootedParentOfPage.relativize(siteRootedPath)
    def parent = this.copy(siteRootedPath=siteRootedPath.parent)
