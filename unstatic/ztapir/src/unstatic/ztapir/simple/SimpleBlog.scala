package unstatic.ztapir.simple

import java.time.{Instant, ZoneId, ZonedDateTime}
import scala.collection.*
import unstatic.{Site, *}
import unstatic.UrlPath.*
import unstatic.ztapir.*
import audiofluidity.rss.{Element, Itemable, LanguageCode, Namespace}

trait SimpleBlog extends ZTBlog:
  object Entry:
    val Presentation  = Blog.EntryPresentation
    type Presentation = Blog.EntryPresentation
    final case class Info (
      mbTitle : Option[String],
      authors : Seq[String],
      tags : Seq[String],
      pubDate : Instant,
      contentType : String,
      mediaPathSiteRooted : Rooted, // from Site root
      permalinkPathSiteRooted : Rooted // from Site root
    )
    final case class Input (
      blog : SimpleBlog,
      site : Site, // duplicative, since it's accessible from SiteLocations. But easy
      renderLocation : SiteLocation,
      mediaLocation : SiteLocation,
      inferredInfo : Entry.Info,
      presentation : Entry.Presentation
    )
  end Entry
  object Layout:
    object Input:
      case class Entry( blog : SimpleBlog, site : Site, renderLocation : SiteLocation, articleContentHtml : String, mbTitle : Option[String], authors : Seq[String], tags : Seq[String], pubDate : Instant, permalinkLocation : SiteLocation, presentation : SimpleBlog.this.Entry.Presentation )
      case class Page( blog : SimpleBlog, site : Site, renderLocation : SiteLocation, mainContentHtml : String )
    end Input
  end Layout

  // you can override this
  val defaultSummaryAsDescriptionMaxLen = 500

  def rssSummaryAsDescription(jsoupDocAbsolutized : org.jsoup.nodes.Document) : String =
    val tmp = jsoupDocAbsolutized.text().take(defaultSummaryAsDescriptionMaxLen)
    val lastSpace = tmp.lastIndexOf(' ')
    (if lastSpace >= 0 then tmp.substring(0, lastSpace) else tmp) + "..."

  def rssItemForResolved(resolved : EntryResolved, fullContent : Boolean) : Element.Item =
    val entryInfo = resolved.entryInfo
    val entryUntemplate = resolved.entryUntemplate
    val absPermalink = site.absFromSiteRooted(resolved.entryInfo.permalinkPathSiteRooted)
    val permalinkRelativeHtml = renderSingleFragment(SiteLocation(entryInfo.permalinkPathSiteRooted), resolved, Entry.Presentation.Rss)
    val jsoupDoc = org.jsoup.Jsoup.parseBodyFragment(permalinkRelativeHtml, absPermalink.parent.toString)
    val jsoupDocAbsolutized = resolveRelativeUrls(jsoupDoc)
    val absolutizedHtml = jsoupDocAbsolutized.body().html
    val summary = rssSummaryAsDescription(jsoupDocAbsolutized)
    val nospamAuthor =
      if entryInfo.authors.nonEmpty then s"""nospam@dev.null (${entryInfo.authors.mkString(", ")})""" else "nospam@dev.null"
    val standardItem =
      Element.Item(
        title = Element.Title(resolved.entryInfo.mbTitle.getOrElse("")),
        link = Element.Link(absPermalink.toString),
        description = Element.Description(summary),
        author = Element.Author(nospamAuthor),
        categories = Nil,
        comments = None,
        enclosure = None,
        guid = Some(Element.Guid(isPermalink = true, absPermalink.toString)),
        pubDate = Some(Element.PubDate( entryInfo.pubDate.atZone(ZoneId.systemDefault()))),
        source = None
      )
    if fullContent then standardItem.withExtra(Element.Content.Encoded(absolutizedHtml)) else standardItem

  // you can override this
  // should remain a def as long as we have lastBuildDate though
  def channelSpec =
    Element.Channel.Spec (
      title              = feedTitle,
      linkUrl            = frontPage.absolutePath.toString,
      description        = feedDescription,
      language           = Some(language),
      lastBuildDate      = Some(ZonedDateTime.now),
      generator          = Some("https://github.com/swaldman/unstatic"),
    )

  // better be def or lazy!
  //
  // if it's a val, it'll blow up trying to fetch the rssFeed SiteLocation before
  // site has been initialized!
  def atomLinkChannelExtra =
    Element.Atom.Link(
      href = rssFeed.absolutePath.toString(),
      rel = Some(Element.Atom.LinkRelation.self),
      `type` = Some("application/rss+xml")
    )

  def rssNamespaces = Namespace.RdfContent :: Namespace.DublinCore :: Namespace.Atom :: Nil

  def feedToXmlSpec : Element.ToXml.Spec = Element.ToXml.Spec.Default

  def makeFeed( fullContent : Boolean = true ) : Element.Rss =
    given Itemable[EntryResolved] with
      extension (resolved : EntryResolved)
        def toItem : Element.Item = rssItemForResolved(resolved, fullContent)

    val instantOrdering = summon[Ordering[Instant]]
    val items =
      ( maxFeedEntries, onlyFeedEntriesSince ) match
        case(Some(max), Some(since)) =>
          entriesResolved
            .filter( resolved => instantOrdering.compare(resolved.entryInfo.pubDate,since) > 0 )
            .take(max)
        case (None, Some(since)) =>
          entriesResolved.filter( resolved => instantOrdering.compare(resolved.entryInfo.pubDate,since) > 0 )
        case (Some(max), None) =>
          entriesResolved.take(max)
        case (None,None) =>
          entriesResolved
    val channel = Element.Channel.create( channelSpec, items ).withExtra( atomLinkChannelExtra )
    Element.Rss(channel).overNamespaces(rssNamespaces)

  // you can override this
  val fullContentFeed = true

  lazy val feed : Element.Rss = makeFeed( fullContentFeed )
  lazy val feedXml : String = feed.asXmlText(feedToXmlSpec)

  val HtmlifierForContentType = immutable.Map[String,Htmlifier] (
    "text/html" -> Htmlifier.identity,
    "text/markdown" -> Flexmark.markdownToHtml,
  )

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

  def maxFeedEntries       : Option[Int]     = maxFrontPageEntries
  def onlyFeedEntriesSince : Option[Instant] = None

  // you must override this
  val feedTitle : String

  // you can override this
  def feedDescription = s"Feed for blog '${feedTitle}' generated by unstatic"

  def feedLinkHtml = s"""<link rel="alternate" type="application/rss+xml" title="${feedTitle}" href="${rssFeed.absolutePath}" />"""

  // you can override this
  val language = LanguageCode.EnglishUnitedStates

  lazy val rssFeed : SiteLocation = SiteLocation(frontPage.siteRootedPath.resolveSibling("feed.rss") )

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

  def entryInput( renderLocation : SiteLocation, resolved : EntryResolved, presentation : Entry.Presentation ) : EntryInput =
    Entry.Input( this, site, renderLocation, SiteLocation(resolved.entryInfo.mediaPathSiteRooted, site), resolved.entryInfo, presentation )

  def permalink( resolved : EntryResolved ) : SiteLocation = SiteLocation( resolved.entryInfo.permalinkPathSiteRooted, site )

  /**
   * Lays out only the entry, the fragment which will later become the main content of the page
   */
  def layoutEntry( input : Layout.Input.Entry ) : String

  def entrySeparator : String

  def layoutPage( input : Layout.Input.Page ) : String

  // TODO: Consider memoizing, since we will do this at least twice at permalink location
  //       (once for permalink, once for feed)
  def renderSingleFragment( renderLocation : SiteLocation, resolved : EntryResolved, presentation : Entry.Presentation ) : String =
    val htmlifier = HtmlifierForContentType(resolved.entryInfo.contentType)
    val ei = entryInput( renderLocation, resolved, presentation )
    val result = resolved.entryUntemplate(ei)
    val htmlifierOptions = Htmlifier.Options(generatorFullyQualifiedName = Some(resolved.entryUntemplate.UntemplateFullyQualifiedName))
    val htmlResult = htmlifier(result.text, htmlifierOptions)
    val info = resolved.entryInfo
    val layoutEntryInput = Layout.Input.Entry(this, site, renderLocation, htmlResult, info.mbTitle, info.authors, info.tags, info.pubDate, SiteLocation(info.permalinkPathSiteRooted,site), presentation )
    layoutEntry( layoutEntryInput )

  def renderSingle( renderLocation : SiteLocation, resolved : EntryResolved ) : String =
    val entry = renderSingleFragment(renderLocation, resolved, Entry.Presentation.Single)
    val layoutPageInput = Layout.Input.Page(this, site, renderLocation, entry)
    layoutPage( layoutPageInput )

  def renderMultiple( renderLocation : SiteLocation, resolveds : immutable.Seq[EntryResolved] ) : String =
    // println("renderMultiple(...)")
    // resolveds.foreach( r => println(s"${r.entryUntemplate} -- ${r.entryInfo.pubDate}") )
    val fragmentTexts = resolveds.map(resolved => renderSingleFragment(renderLocation, resolved, Entry.Presentation.Multiple))
    val unifiedFragmentTexts = fragmentTexts.mkString(entrySeparator)
    val layoutPageInput = Layout.Input.Page(this, site, renderLocation, unifiedFragmentTexts)
    layoutPage( layoutPageInput )

  def renderRange(renderLocation: SiteLocation, from: Instant, until: Instant): String =
    val ordering = summon[Ordering[Instant]]
    val rs = entriesResolved.filter(r => ordering.gteq(from, r.entryInfo.pubDate) && ordering.lt(r.entryInfo.pubDate, until))
    renderMultiple(renderLocation, rs.toVector)

  def renderSince(renderLocation: SiteLocation, from: Instant): String =
    renderRange( renderLocation, from, Instant.now)

  override def endpointBindings : immutable.Seq[ZTEndpointBinding] =
    super.endpointBindings :+ ZTEndpointBinding.publicReadOnlyRss( rssFeed, zio.ZIO.attempt( feedXml ))

