package unstatic.ztapir.simple

import java.time.Instant
import scala.collection.*
import unstatic.{Site, *}
import unstatic.UrlPath.*
import unstatic.ztapir.*

trait SimpleBlog extends ZTBlog:
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
    final case class Input(renderPath : SiteLocation, mediaPath : SiteLocation, presentationMultiple : Boolean)
  end Entry
  object Layout:
    object Input:
      case class Entry( renderLocation : SiteLocation, articleContentHtml : String, mbTitle : Option[String], authors : Seq[String], tags : Seq[String], pubDate : Instant, permalinkLocation : ZTSite#SiteLocation, presentationMultiple : Boolean )
      case class Page( renderLocation : SiteLocation, mainContentHtml : String )
    end Input
  end Layout

  type EntryInfo      = Entry.Info
  type EntryInput     = Entry.Input
  type EntryMetadata  = Nothing

  /**
   * Reverse-chronological!
   */
  given entryOrdering : Ordering[EntryResolved] =
    Ordering.by( (er : EntryResolved) => (er.entryInfo.pubDate, er.entryUntemplate.UntemplatePackage, er.entryUntemplate.UntemplateName) ).reverse

  val site                : Site // the type is Blog.this.Site, narrowed to ZTSite by ZTBlog
  val frontPage           : SiteLocation
  val maxFrontPageEntries : Int

  /**
   * Filter the index of your untemplates for the blog's entries
   */
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
  end entryInfo

  def entryInput( renderLocation : SiteLocation, resolved : EntryResolved, presentationMultiple : Boolean ) : EntryInput =
    Entry.Input( renderLocation, SiteLocation(resolved.entryInfo.mediaPathSiteRooted, site), presentationMultiple )

  def permalink( resolved : EntryResolved ) : SiteLocation = SiteLocation( resolved.entryInfo.permalinkPathSiteRooted, site )

  /**
   * Lays out only the entry, the fragment which will later become the main content of the page
   */
  def layoutEntry( input : Layout.Input.Entry ) : String

  def entrySeparator : String

  def layoutPage( input : Layout.Input.Page ) : String

  def renderSingleFragment( renderLocation : SiteLocation, resolved : EntryResolved, presentationMultiple : Boolean ) : String =
    val renderer = ContentRendererForContentType(resolved.entryInfo.contentType)
    val ei = entryInput( renderLocation, resolved, presentationMultiple )
    val result = resolved.entryUntemplate(ei)
    val renderResult = renderer(result)
    val info = resolved.entryInfo
    val layoutEntryInput = Layout.Input.Entry(renderLocation, renderResult.text, info.mbTitle, info.authors, info.tags, info.pubDate, SiteLocation(info.permalinkPathSiteRooted,site), presentationMultiple )
    layoutEntry( layoutEntryInput )

  def renderSingle( renderLocation : SiteLocation, resolved : EntryResolved ) : String =
    val entry = renderSingleFragment(renderLocation, resolved, false)
    val layoutPageInput = Layout.Input.Page(renderLocation, entry)
    layoutPage( layoutPageInput )

  def renderMultiple( renderLocation : SiteLocation, resolveds : immutable.Seq[EntryResolved] ) : String =
    val fragmentTexts = resolveds.map(resolved => renderSingleFragment(renderLocation, resolved, true))
    val unifiedFragmentTexts = fragmentTexts.mkString(entrySeparator)
    val layoutPageInput = Layout.Input.Page(renderLocation, unifiedFragmentTexts)
    layoutPage( layoutPageInput )

  def renderRange(renderLocation: SiteLocation, from: Instant, until: Instant): String =
    val ordering = summon[Ordering[Instant]]
    val rs = entriesResolved.filter(r => ordering.gteq(from, r.entryInfo.pubDate) && ordering.lt(r.entryInfo.pubDate, until))
    renderMultiple(renderLocation, rs.toVector)

  def renderSince(renderLocation: SiteLocation, from: Instant): String =
    renderRange( renderLocation, from, Instant.now)

