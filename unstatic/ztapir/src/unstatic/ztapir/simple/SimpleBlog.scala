package unstatic.ztapir.simple

import java.time.Instant
import scala.collection.*
import unstatic.{Site, *}
import unstatic.UrlPath.*
import unstatic.ztapir.*

object SimpleBlog:
  object Entry:
    final case class Info (
      mbTitle : Option[String],
      authors : Seq[String],
      tags : Seq[String],
      pubDate : Instant,
      contentType : String,
      mediaPathSiteRooted : Rooted, // from Site root
      permalinkPathSiteRooted : Rooted // from SiteRoot
    )
    final case class Input(renderPath : ZTSite#SiteLocation, mediaPath : ZTSite#SiteLocation, presentationMultiple : Boolean)
trait SimpleBlog extends ZTBlog:
  import SimpleBlog.*

  type EntryInfo      = Entry.Info
  type EntryInput     = Entry.Input
  type EntryMetadata  = Nothing

  /**
   * Reverse-chronological!
   */
  given entryOrdering : Ordering[EntryResolved] =
    Ordering.by( (er : EntryResolved) => (er.entryInfo.pubDate, er.entryUntemplate.UntemplatePackage, er.entryUntemplate.UntemplateName) ).reverse

  val site                : Site
  val frontPage           : SiteLocation
  val maxFrontPageEntries : Int

  def entryUntemplates : immutable.Set[EntryUntemplate]

  def mediaPathPermalink( checkable : Attribute.Checkable, ut : untemplate.Untemplate[?,?] ) : MediaPathPermalink

  def entryInfo( template : EntryUntemplate ) : EntryInfo =
    import Attribute.Key.*

    val checkable : Attribute.Checkable = Attribute.Checkable.from(template)
    val mbTitle     = checkable.check(`Title`)
    val authors     = checkable.check(`Author`).getOrElse(Nil)
    val tags        = checkable.check(`Tag`).getOrElse(Nil)
    val pubDate     = checkable.check(`PubDate`).getOrElse( throw missingAttribute( template, `PubDate`) )
    val contentType = findContentType( checkable, template )

    val MediaPathPermalink( mediaPathSiteRooted, permalinkSiteRooted ) = mediaPathPermalink( checkable, template )

    Entry.Info(mbTitle, authors, tags, pubDate, contentType, mediaPathSiteRooted, permalinkSiteRooted)


  def entryInput( renderLocation : SiteLocation, resolved : EntryResolved, presentationMultiple : Boolean ) : EntryInput =
    Entry.Input( renderLocation, SiteLocation(resolved.entryInfo.mediaPathSiteRooted, site), presentationMultiple )

  def permalink( resolved : EntryResolved ) : SiteLocation = SiteLocation( resolved.entryInfo.permalinkPathSiteRooted, site )

  def renderSingle( renderLocation : SiteLocation, resolved : EntryResolved ) : String

  def renderMultiple( renderLocation : SiteLocation, resolveds : immutable.Seq[EntryResolved] ) : String