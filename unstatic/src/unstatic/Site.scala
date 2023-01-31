package unstatic

import scala.collection.*
import unstatic.UrlPath.*

object Site:
  trait Composite extends Site:
    def locationBindingSources : immutable.Seq[StaticLocationBinding.Source]

    def locationBindings : immutable.Seq[StaticLocationBinding] = locationBindingSources.flatMap( _.locationBindings )
  end Composite

trait Site extends StaticLocationBinding.Source:
  def serverUrl : Abs
  def basePath  : Rooted
  def sitePath  : Abs = serverUrl.embedRoot(basePath)

  def siteRoot = sitePath

  def serverRootedPath( fromSiteRootedPath : Rooted ) : Rooted = basePath.embedRoot( fromSiteRootedPath )
  def serverRootedPath( fromSiteRootedPath : String ) : Rooted = serverRootedPath( Rooted(fromSiteRootedPath) )

  def absFromServerRooted( fromServerRootedPath : Rooted ) : Abs = serverUrl.serverRoot.embedRoot(fromServerRootedPath )
  def absFromServerRooted( fromServerRootedPath : String ) : Abs = absFromServerRooted( Rooted(fromServerRootedPath) )

  def absFromSiteRooted( fromSiteRootedPath : Rooted ) : Abs = absFromServerRooted(serverRootedPath(fromSiteRootedPath))
  def absFromSiteRooted( fromSiteRootedPath : String ) : Abs = absFromSiteRooted( Rooted( fromSiteRootedPath ) )

  case class SiteLocation( siteRootedPath : Rooted, site : this.type = Site.this ):
    def relative(using base: PageBase) : Rel          = base.siteRootedParentOfPage.relativize(siteRootedPath)
    lazy val serverRootedPath          : Rooted       = site.serverRootedPath(siteRootedPath)
    lazy val absolutePath              : Abs          = absFromServerRooted( this.serverRootedPath )
    lazy val parent                    : SiteLocation = this.copy(siteRootedPath=siteRootedPath.parent)

  lazy val siteRootLocation = SiteLocation( Rooted.root )
