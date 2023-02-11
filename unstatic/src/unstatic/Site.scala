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

  def siteRootedPath( fromServerRootedPath : Rooted ) : Rooted =
    if basePath.isRoot then
      fromServerRootedPath
    else
      val prefix = fromServerRootedPath.elements.take(basePath.elements.length)
      if prefix != basePath.elements then
        throw new PathNotFromSite( s"Server rooted path '${fromServerRootedPath}' is not from site with base path '${basePath}'")
      else
        Rooted.fromElements(fromServerRootedPath.elements.drop(basePath.elements.length) : _*)
  def siteRootedPath( fromServerRootedPath : String ) : Rooted = siteRootedPath( Rooted(fromServerRootedPath) )

  def absFromServerRooted( fromServerRootedPath : Rooted ) : Abs = serverUrl.serverRoot.embedRoot(fromServerRootedPath )
  def absFromServerRooted( fromServerRootedPath : String ) : Abs = absFromServerRooted( Rooted(fromServerRootedPath) )

  def absFromSiteRooted( fromSiteRootedPath : Rooted ) : Abs = absFromServerRooted(serverRootedPath(fromSiteRootedPath))
  def absFromSiteRooted( fromSiteRootedPath : String ) : Abs = absFromSiteRooted( Rooted( fromSiteRootedPath ) )

  def location( siteRootedPath : Rooted ) : SiteLocation = SiteLocation( siteRootedPath )
  def location( siteRootedPath : String ) : SiteLocation = location(Rooted.parseAndRoot( siteRootedPath ))

  case class SiteLocation( siteRootedPath : Rooted, site : this.type = Site.this ):
    def relative(using base: PageBase) : Rel          = base.siteRootedParentOfPage.relativize(siteRootedPath)
    lazy val serverRootedPath          : Rooted       = site.serverRootedPath(siteRootedPath)
    lazy val absolutePath              : Abs          = absFromServerRooted( this.serverRootedPath )
    lazy val parent                    : SiteLocation = this.copy(siteRootedPath=siteRootedPath.parent)
    override def toString()            : String       = this.serverRootedPath.toString()

  lazy val siteRootLocation = SiteLocation( Rooted.root )

  def allBindings : immutable.Seq[AnyBinding] = this.locationBindings