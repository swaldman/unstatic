package unstatic

import UrlPath.*

object PageBase:
  def fromPage( siteRootedPagePath : Rooted )                    : PageBase = PageBase(siteRootedPagePath.parent)
  def fromPage( siteRootedPageLocation : Site#SiteLocation)      : PageBase = fromPage(siteRootedPageLocation.siteRootedPath)
  def fromParent( siteRootedParentPath : Rooted )                : PageBase = PageBase(siteRootedParentPath)
  def fromParent( siteRootedParentLocation : Site#SiteLocation ) : PageBase = fromParent(siteRootedParentLocation.siteRootedPath)
  private def apply( siteRootedParentOfPage : Rooted )           : PageBase = new PageBase(siteRootedParentOfPage)
case class PageBase( siteRootedParentOfPage : Rooted ):
  def relativize( siteRootedPath : Rooted ) : Rel = this.siteRootedParentOfPage.relativize(siteRootedPath)