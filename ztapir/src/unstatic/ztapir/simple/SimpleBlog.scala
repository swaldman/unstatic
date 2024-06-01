package unstatic.ztapir.simple

import java.time.{Instant, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import DateTimeFormatter.ISO_INSTANT
import scala.collection.immutable
import unstatic.{Site, *}
import unstatic.UrlPath.*
import unstatic.ztapir.*
import audiofluidity.rss.{Element, Itemable, LanguageCode, Namespace}

import com.mchange.conveniences.boolean.*

object SimpleBlog:
  object Htmlifier:
    val identity : Htmlifier = (s : String, opts : Options) => s
    val preText : Htmlifier = (s : String, opts : Options) => s"<pre>${s}</pre>"
    val defaultMarkdown : Htmlifier = (s : String, opts : Options) => Flexmark.defaultMarkdownToHtml(s, opts.generatorFullyQualifiedName )
    case class Options( generatorFullyQualifiedName : Option[String] )
  type Htmlifier = Function2[String,Htmlifier.Options,String]
  object Rss:
    private def rssSummaryAsDescription(jsoupDocAbsolutized : org.jsoup.nodes.Document, maxLen : Int) : String =
      val tmp = jsoupDocAbsolutized.text().take(maxLen)
      val lastSpace = tmp.lastIndexOf(' ')
      (if lastSpace >= 0 then tmp.substring(0, lastSpace) else tmp) + "..."

    private def absolutizedHtmlSummary(blog : SimpleBlog)( permalinkRelativeHtml : String, absPermalink : Abs ) : (String, String) =
      val jsoupDoc = org.jsoup.Jsoup.parseBodyFragment(permalinkRelativeHtml, absPermalink.parent.toString)
      mutateResolveRelativeUrls(jsoupDoc)
      (jsoupDoc.body().html, rssSummaryAsDescription(jsoupDoc,blog.defaultSummaryAsDescriptionMaxLen))

    private def authorElementMbCreatorElement( authorSeq : Seq[String] ) : (Element.Author, Option[Element.DublinCore.Creator]) =
      val authorsString : Option[String] =
        authorSeq.length match
          case 0 => None
          case 1 => Some( authorSeq.head )
          case 2 => Some( authorSeq.head + " and " + authorSeq.last )
          case n =>
            val anded = authorSeq.init :+ s"and ${authorSeq.last}"
            Some( anded.mkString(", ") )
      val nospamAuthorElem =
        Element.Author( authorsString.fold("nospam@dev.null")(as => s"nospam@dev.null (${as})") )
      val mbDcCreatorElem =
        authorsString.map( as => Element.DublinCore.Creator( as ) )
      ( nospamAuthorElem, mbDcCreatorElem )  

    def rssItem( blog : SimpleBlog )(
      resolved : blog.EntryResolved,
      fullContent : Boolean,
      extraChildren : List[audiofluidity.rss.Element[?]] = Nil,
      extraChildrenRaw : List[scala.xml.Elem] = Nil
    ) : Element.Item =
      val entryInfo = resolved.entryInfo
      val permalinkRelativeHtml = blog.renderSingleFragment(blog.site.location(entryInfo.permalinkPathSiteRooted), resolved, blog.Entry.Presentation.Rss)
      val (absolutizedHtml, summary) = absolutizedHtmlSummary(blog)( permalinkRelativeHtml, entryInfo.absPermalink )
      val (authorElement, mbCreatorElement) = authorElementMbCreatorElement( entryInfo.authors )
      val mbTitleElement = entryInfo.mbTitle.map( title => Element.Title( title ) )
      val linkElement = Element.Link(entryInfo.absPermalink.toString)
      val guidElement = Element.Guid(isPermalink = true, entryInfo.absPermalink.toString)

      val standardItem =
        Element.Item(
          title = mbTitleElement,
          link = Some(linkElement),
          description = Some(Element.Description(summary)),
          author = Some(authorElement),
          categories = Nil,
          comments = None,
          enclosure = None,
          guid = Some(guidElement),
          pubDate = Some(Element.PubDate(entryInfo.pubDate.atZone(blog.timeZone))),
          source = None
        )
      val baseItem =
        val withCreator = mbCreatorElement.fold( standardItem )(dcce => standardItem.withExtra( dcce ))
        val withUpdated = entryInfo.updateHistory.headOption.fold( withCreator )( ur => withCreator.withExtra( Element.Atom.Updated( ur.timestamp ) ) )
        val withFullContent = if fullContent then withUpdated.withExtra(Element.Content.Encoded(absolutizedHtml)) else withUpdated
        withFullContent
      baseItem.withExtras( extraChildren ).withExtras( extraChildrenRaw )

    def defaultChannelSpecNow( blog : SimpleBlog ) : Element.Channel.Spec =
      Element.Channel.Spec (
        title              = blog.feedTitle,
        linkUrl            = blog.frontPage.absolutePath.toString,
        description        = blog.feedDescription,
        language           = Some( blog.language ),
        lastBuildDate      = Some( ZonedDateTime.now(blog.timeZone) ),
        generator          = Some( "https://github.com/swaldman/unstatic" ),
      )

    def atomLinkChannelExtra( blog : SimpleBlog ) : Element.Atom.Link =
      Element.Atom.Link(
        href = blog.rssFeed.absolutePath.toString(),
        rel = Some(Element.Atom.LinkRelation.self),
        `type` = Some("application/rss+xml")
      )

    val DefaultRssNamespaces = Namespace.RdfContent :: Namespace.DublinCore :: Namespace.Atom :: Namespace.Iffy :: Nil

    val DefaultFeedToXmlSpec : Element.ToXml.Spec = Element.ToXml.Spec.Default

    val DefaultRssFeedIdentifiers = immutable.Set("rssFeed")

    def defaultItemable( blog : SimpleBlog ) : Itemable[blog.EntryResolved] =
      new Itemable[blog.EntryResolved]:
        extension (resolved : blog.EntryResolved)
          def toItem : Element.Item = rssItem( blog )(resolved, blog.fullContentFeed)

    def makeDefaultFeed( blog : SimpleBlog ) : Element.Rss =
      makeFeed( blog )( defaultItemable( blog ), blog.maxFeedEntries, blog.onlyFeedEntriesSince, defaultChannelSpecNow( blog ), DefaultRssNamespaces, blog.entriesResolved )

    def makeFeed( blog : SimpleBlog )(
      itemable                 : Itemable[blog.EntryResolved],
      maxEntries               : Option[Int],
      onlySince                : Option[Instant],
      channelSpec              : Element.Channel.Spec,
      rssNamespaces            : List[Namespace],
      candidateEntriesResolved : immutable.SortedSet[blog.EntryResolved],
      extraChannelChildren     : List[Element[?]]     = Nil,
      extraChannelChildrenRaw  : List[scala.xml.Elem] = Nil,
      extraRssChildren         : List[Element[?]]     = Nil,
      extraRssChildrenRaw      : List[scala.xml.Elem] = Nil
    ) : Element.Rss =
        given Itemable[blog.EntryResolved] = itemable
        val instantOrdering = summon[Ordering[Instant]]
        val items =
          ( maxEntries, onlySince ) match
            case(Some(max), Some(since)) =>
              candidateEntriesResolved
                .filter( resolved => instantOrdering.compare(resolved.entryInfo.pubDate,since) > 0 )
                .take(max)
            case (None, Some(since)) =>
              candidateEntriesResolved.filter( resolved => instantOrdering.compare(resolved.entryInfo.pubDate,since) > 0 )
            case (Some(max), None) =>
              candidateEntriesResolved.take(max)
            case (None,None) =>
              candidateEntriesResolved
        val channel =
          val tmp = Element.Channel.create( channelSpec, items ).withExtra( atomLinkChannelExtra(blog) )
          val completenessValue =
            if tmp.items.forall( _.guid.nonEmpty ) then
              if blog.fullContentFeed then
                Element.Iffy.Completeness.Value.Content
              else
                Element.Iffy.Completeness.Value.Metadata
            else
              Element.Iffy.Completeness.Value.Ping
          val completeness = Element.Iffy.Completeness( completenessValue )
          tmp.withExtra(completeness).withExtras( extraChannelChildren ).withExtras( extraChannelChildrenRaw )
        Element.Rss(channel).overNamespaces(rssNamespaces).withExtras( extraRssChildren ).withExtras( extraRssChildrenRaw )

    private val SproutSuffixFormatterBase = DateTimeFormatter.ofPattern("""-yyyy'-'MM'-'dd'-'HH'-'mm'-'ss""")

    def makeSproutFeed( blog : SimpleBlog )(
      sprout                   : blog.EntryResolved,
      maxEntries               : Option[Int],
      onlySince                : Option[Instant],
      channelSpec              : Element.Channel.Spec,
      rssNamespaces            : List[Namespace],
      extraChannelChildren     : List[Element[?]]     = Nil,
      extraChannelChildrenRaw  : List[scala.xml.Elem] = Nil,
      extraRssChildren         : List[Element[?]]     = Nil,
      extraRssChildrenRaw      : List[scala.xml.Elem] = Nil
    ) : Element.Rss =
      val suffixFormatter = SproutSuffixFormatterBase.withZone( blog.timeZone )
      // absolutizedHtmlSummary( permalinkRelativeHtml : String, absPermalink : Abs ) : (String, String)
      val info = sprout.entryInfo
      val updates = blog.updateRecordsForDisplayFromSiteRoot(info)
      val guidBase =
        info.absPermalink.path.elements.lastOption match
          case None => info.absPermalink.resolve("sprout").toString()
          case Some(fn) =>
            val lastDot = fn.lastIndexOf('.')
            val modfn = if lastDot >= 0 then fn.substring(0, lastDot) else fn
            info.absPermalink.resolveSibling( modfn + "_sprout" ).toString

      def descElement(urfd : UpdateRecord.ForDisplay) = Element.Description( urfd.description.getOrElse(s"New major update, ${blog.dateTimeFormatter.format(urfd.timestamp)}") )
      val (revisions, base) = ( updates.init, updates.last ) // would splitAt be more efficient?
      val baseItem =
        val baseUnfinished = revisions.isEmpty
        val titleTag = baseUnfinished.tf("Working Draft")("Seed")
        val linkElement = Element.Link(base.finalMinorRevisionRelative.fold(info.absPermalink.toString + "#sprout" + suffixFormatter.format(info.pubDate))(rel => blog.site.absFromSiteRooted(Rooted.root.resolve(rel))).toString)
        val (authorElement, mbCreatorElement) = authorElementMbCreatorElement( info.authors )
        val guidElement = Element.Guid(isPermalink = false, guidBase + suffixFormatter.format( info.pubDate ) )
        val baseBase =
          Element.Item(
            title = info.mbTitle.map( title => Element.Title( s"[${titleTag}] ${title}" ) ),
            link = Some(linkElement),
            description = Some(descElement(base)),
            author = Some(authorElement),
            categories = Nil,
            comments = None,
            enclosure = None,
            guid = Some(guidElement),
            pubDate = Some(Element.PubDate(info.pubDate.atZone(blog.timeZone))),
            source = None
          )
        mbCreatorElement.fold( baseBase )( creator => baseBase.withExtra( creator ) )  
      ???

  end Rss
end SimpleBlog

trait SimpleBlog extends ZTBlog:

  import SimpleBlog.Htmlifier

  object Entry:
    val Presentation  = Blog.EntryPresentation
    type Presentation = Blog.EntryPresentation
    final case class Info (
      mbTitle : Option[String],
      authors : Seq[String],
      tags : Seq[String],
      pubDate : Instant,
      updateHistory : immutable.SortedSet[UpdateRecord],
      sprout : Boolean,
      mbAnchor : Option[String],
      contentType : String,
      mediaPathSiteRooted : Rooted, // from Site root
      permalinkPathSiteRooted : Rooted // from Site root
    ):
      val absPermalink = site.absFromSiteRooted(permalinkPathSiteRooted)


    final case class Input (
      blog : SimpleBlog,
      site : Site, // duplicative, since it's accessible from SiteLocations. But easy
      renderLocation : SiteLocation,
      mediaLocation : SiteLocation,
      inferredInfo : Entry.Info,
      presentation : Entry.Presentation
    ):
      def entryById( id : String ) : EntryResolved = SimpleBlog.this.entryById(id)
  end Entry

  object Layout:
    object Input:
      case class Entry(
        blog : SimpleBlog,
        site : Site,
        renderLocation : SiteLocation,
        articleContentHtml : String,
        info : EntryInfo,
        sourceEntry : EntryResolved,
        previousEntry : Option[EntryResolved],
        nextEntry : Option[EntryResolved],
        presentation : SimpleBlog.this.Entry.Presentation
      )
      case class Page( blog : SimpleBlog, site : Site, renderLocation : SiteLocation, mainContentHtml : String, sourceEntries : immutable.Seq[EntryResolved] )
    end Input
  end Layout

  // you can override this
  val timeZone: ZoneId = ZoneId.systemDefault()

  // you can override these
  lazy val dayOnlyFormatter  = DateTimeFormatter.ofPattern("""yyyy'-'MM'-'dd""").withZone(timeZone)
  lazy val hourOnlyFormatter = DateTimeFormatter.ofPattern("""hh':'mm' 'a' 'zzz""").withZone(timeZone)
  lazy val dateTimeFormatter = DateTimeFormatter.ofPattern("""yyyy'-'MM'-'dd' @ 'hh':'mm' 'a' 'zzz""").withZone(timeZone)

  // you can override this
  val defaultSummaryAsDescriptionMaxLen = 500

  // you can override this
  val fullContentFeed = true

  lazy val feed          : Element.Rss              = SimpleBlog.Rss.makeDefaultFeed( this )
  lazy val feedToXmlSpec : Element.ToXml.Spec       = SimpleBlog.Rss.DefaultFeedToXmlSpec
  lazy val feedXml       : String                   = feed.asXmlText(feedToXmlSpec)
  lazy val feedBytes     : immutable.ArraySeq[Byte] = immutable.ArraySeq.unsafeWrapArray( feedXml.getBytes(CharsetUTF8) )

  private val DefaultHtmlifierForContentType = immutable.Map[String,Htmlifier] (
    "text/html"     -> Htmlifier.identity,
    "text/markdown" -> Htmlifier.defaultMarkdown,
    "text/plain"    -> Htmlifier.preText
  )

  // you can override this
  def htmlifierForContentType(contentType : String) : Option[Htmlifier] =
    DefaultHtmlifierForContentType.get( contentType )

  type EntryInfo      = Entry.Info
  type EntryInput     = Entry.Input
  type EntryMetadata  = Nothing

  /**
   * Reverse-chronological!
   */
  given entryOrdering : Ordering[EntryResolved] =
    Ordering.by( (er : EntryResolved) => (er.entryInfo.pubDate, er.entryUntemplate.UntemplateFullyQualifiedName) ).reverse

  val site                : Site // the type is Blog.this.Site, narrowed to ZTSite by ZTBlog
  val frontPage           : SiteLocation

  def maxFeedEntries       : Option[Int]     = maxFrontPageEntries
  def onlyFeedEntriesSince : Option[Instant] = None

  // you must override this
  val feedTitle : String

  // you can override this
  def feedDescription = s"Feed for blog '${feedTitle}', generated by unstatic"

  def feedLinkHtml = s"""<link rel="alternate" type="application/rss+xml" title="${feedTitle}" href="${rssFeed.absolutePath}" />"""

  // you can override this
  val language = LanguageCode.EnglishUnitedStates

  lazy val rssFeed : SiteLocation = SiteLocation(frontPage.siteRootedPath.resolveSibling("feed.rss") )

  // you can override this
  def defaultAuthors : immutable.Seq[String] = Nil

  // you can override this
  def rssFeedIdentifiers = SimpleBlog.Rss.DefaultRssFeedIdentifiers

  // you can override this
  val revisionBinder : Option[RevisionBinder] = None

  // you can override this, but in only has effect if some revisionBinder is set as well
  val diffBinder : Option[DiffBinder] = None

  /**
   * Filter the index of your untemplates for the blog's entries
   */
  def entryUntemplates : immutable.Set[EntryUntemplate]

  def mediaPathPermalink( ut : untemplate.Untemplate[?,?] ) : MediaPathPermalink

  def entryInfo( template : EntryUntemplate ) : EntryInfo =
    import Attribute.Key

    val mbTitle                   = Key.`Title`.caseInsensitiveCheck(template)
    val authors                   = Key.`Author`.caseInsensitiveCheck(template).getOrElse(defaultAuthors)
    val tags                      = Key.`Tag`.caseInsensitiveCheck(template).getOrElse(Nil)
    val pubDate                   = Key.`PubDate`.caseInsensitiveCheck(template).getOrElse( throw missingAttribute( template, Key.`PubDate`) )
    val updateHistory             = Key.`UpdateHistory`.caseInsensitiveCheck(template).getOrElse( immutable.SortedSet.empty[UpdateRecord] )
    val sprout                    = Key.`Sprout`.caseInsensitiveCheck(template).getOrElse( false )
    val mbAnchor                  = Key.`Anchor`.caseInsensitiveCheck(template)
    val contentType               = normalizeContentType( findContentType( template ) )

    val MediaPathPermalink( mediaPathSiteRooted, permalinkSiteRooted ) = mediaPathPermalink( template )

    Entry.Info(mbTitle, authors, tags, pubDate, updateHistory, sprout, mbAnchor, contentType, mediaPathSiteRooted, permalinkSiteRooted)
  end entryInfo

  private def updateRecordsForDisplay( renderedFrom : Rooted, permalinkPathSiteRooted : Rooted, updateHistorySorted : immutable.SortedSet[UpdateRecord], initialPubDate : Instant ) : Seq[UpdateRecord.ForDisplay] =
    val initialNoLatestMinorRevision = UpdateRecord.ForDisplay( initialPubDate, Some("Initial publication."), None, None, None, None, None )
    if updateHistorySorted.nonEmpty then
      def relativize( rooted : Rooted ) = renderedFrom.relativizeFromNearestDir(rooted)
      lazy val updateRecordToLatestMinorRevision = nonCurrentUpdateRecordToOwnLatestMinorRevisionSpec( updateHistorySorted )
      val updateHistory = updateHistorySorted.toVector
      val current = updateHistory.head
      val allExceptInitial =
        updateHistory.map: ur =>
          this.revisionBinder match
            case Some(rb) =>
              val supercededRevisionRelative = ur.supercededRevisionSpec.map( srs => relativize(rb.revisionPathFinder(permalinkPathSiteRooted,srs)) )
              val finalMinorRevisionSpec = updateRecordToLatestMinorRevision.get(ur)
              val finalMinorRevisionRelative = finalMinorRevisionSpec.map( lmrs => relativize(rb.revisionPathFinder(permalinkPathSiteRooted,lmrs)) )
              val diffRel =
                for
                  db <- diffBinder
                  srs <- ur.supercededRevisionSpec
                yield
                  relativize(db.diffPathFinder(permalinkPathSiteRooted,srs,finalMinorRevisionSpec))
              UpdateRecord.ForDisplay( ur.timestamp, ur.description, finalMinorRevisionSpec, ur.supercededRevisionSpec, finalMinorRevisionRelative, supercededRevisionRelative, diffRel )
            case None =>  
              UpdateRecord.ForDisplay( ur.timestamp, ur.description, None, ur.supercededRevisionSpec, None, None, None )
      val firstRealUpdate = allExceptInitial.last
      val initial = initialNoLatestMinorRevision.copy( finalMinorRevisionSpec = firstRealUpdate.supercededRevisionSpec, finalMinorRevisionRelative = firstRealUpdate.supercededRevisionRelative )
      allExceptInitial :+ initial
    else
      Seq(initialNoLatestMinorRevision)

  def updateRecordsForDisplayFromSiteRoot( info : Entry.Info ) = updateRecordsForDisplay( Rooted.root, info )

  def updateRecordsForDisplay( renderedFrom : Rooted, info : Entry.Info ) : Seq[UpdateRecord.ForDisplay] =
    updateRecordsForDisplay(renderedFrom,info.permalinkPathSiteRooted,info.updateHistory,info.pubDate)

  def updateRecordsForDisplay( renderedFrom : SiteLocation, info : Entry.Info ) : Seq[UpdateRecord.ForDisplay] = updateRecordsForDisplay( renderedFrom.siteRootedPath, info )

  def updateRecordsForDisplay( layoutInputEntry : Layout.Input.Entry ) : Seq[UpdateRecord.ForDisplay] = updateRecordsForDisplay( layoutInputEntry.renderLocation, layoutInputEntry.info )

  def priorRevisionSiteRooted( info : Entry.Info ) : Option[Rooted] = priorRevisionSiteRooted( info.updateHistory, info.permalinkPathSiteRooted )

  def priorRevisionSiteRooted( updateHistory : immutable.SortedSet[UpdateRecord], permalinkPathSiteRooted : Rooted ) : Option[Rooted] =
    updateHistory.headOption.flatMap( ur => priorRevisionSiteRooted( ur, permalinkPathSiteRooted ) )

  def priorRevisionSiteRooted( updateRecord : UpdateRecord, permalinkPathSiteRooted : Rooted ) : Option[Rooted] =
    for
      rb <- this.revisionBinder
      rs <- updateRecord.supercededRevisionSpec
    yield
      rb.revisionPathFinder( permalinkPathSiteRooted, rs )

  def entryInput( renderLocation : SiteLocation, resolved : EntryResolved, presentation : Entry.Presentation ) : EntryInput =
    Entry.Input( this, site, renderLocation, SiteLocation(resolved.entryInfo.mediaPathSiteRooted, site), resolved.entryInfo, presentation )

  def permalink( resolved : EntryResolved ) : SiteLocation = SiteLocation( resolved.entryInfo.permalinkPathSiteRooted, site )

  def mediaDir( resolved : EntryResolved ) : SiteLocation = SiteLocation( resolved.entryInfo.mediaPathSiteRooted, site )

  override def identifiers( resolved : EntryResolved ) : immutable.Set[String] =
    super.identifiers(resolved) ++ Attribute.Key.`Anchor`.caseInsensitiveCheck(resolved.entryUntemplate)

  /**
   * Lays out only the entry, the fragment which will later become the main content of the page
   */
  def layoutEntry( input : Layout.Input.Entry ) : String

  def entrySeparator : String

  def layoutPage( input : Layout.Input.Page ) : String

  // see https://github.com/scala/bug/issues/12727
  def previous( resolved : EntryResolved ) : Option[EntryResolved] = entriesResolved.rangeFrom(resolved).tail.headOption

  def next( resolved : EntryResolved ) : Option[EntryResolved] = entriesResolved.maxBefore(resolved)

  def renderSingleFragment( renderLocation : SiteLocation, resolved : EntryResolved, presentation : Entry.Presentation ) : String =
    val contentType = resolved.entryInfo.contentType
    val htmlifier = htmlifierForContentType(contentType).getOrElse {
      throw new NoHtmlifierForContentType( s"Could not find a function to convert entries of Content-Type '${contentType}' into HTML.")
    }
    val ei = entryInput( renderLocation, resolved, presentation )
    val result = resolved.entryUntemplate(ei)
    val htmlifierOptions = Htmlifier.Options(generatorFullyQualifiedName = Some(resolved.entryUntemplate.UntemplateFullyQualifiedName))
    val htmlResult = htmlifier(result.text, htmlifierOptions)
    val info = resolved.entryInfo
    val layoutEntryInput = Layout.Input.Entry(this, site, renderLocation, htmlResult, info, resolved, previous(resolved), next(resolved), presentation )
    val hashSpecialsUnresolvedHtml = layoutEntry( layoutEntryInput )
    if entryFragmentsResolveHashSpecials then
      val resolveEscapes = // we only want to do this once for each piece of text
        presentation match
          case Entry.Presentation.Single   => !entryTopLevelResolveHashSpecials
          case Entry.Presentation.Multiple => !multipleTopLevelResolveHashSpecials
          case Entry.Presentation.Rss      => true // there is never a potential higher-level resolver for RSS fragments
      val sourceId = s"entry-fragment[${presentation}](source=${resolved.entryUntemplate.UntemplateName}, endpoint=${renderLocation.siteRootedPath})"
      site.htmlFragmentResolveHashSpecials(sourceId, renderLocation.siteRootedPath, hashSpecialsUnresolvedHtml, Some(resolved.entryInfo.mediaPathSiteRooted), resolveEscapes)
    else
      hashSpecialsUnresolvedHtml

  def renderSingle( renderLocation : SiteLocation, resolved : EntryResolved ) : String =
    val entry = renderSingleFragment(renderLocation, resolved, Entry.Presentation.Single)
    val layoutPageInput = Layout.Input.Page(this, site, renderLocation, entry, immutable.Seq(resolved))
    layoutPage( layoutPageInput )

  def nonCurrentUpdateRecordToOwnLatestMinorRevisionSpec( updateHistory : immutable.SortedSet[UpdateRecord] ) : Map[UpdateRecord,String] =
    if updateHistory.nonEmpty then
      val uhl = updateHistory.toList
      uhl.tail.zip(uhl)
        .collect { case (ur, UpdateRecord(_,_,Some(lastCurrentVersionSpec))) => (ur, lastCurrentVersionSpec) }
        .toMap
    else
      Map.empty

  // you can override this
  def renderMultiplePrologue : String = ""

  // you can override this
  def renderMultipleEpilogue : String = ""

  def renderMultiple( renderLocation : SiteLocation, resolveds : immutable.Seq[EntryResolved] ) : String =
    // println("renderMultiple(...)")
    // resolveds.foreach( r => println(s"${r.entryUntemplate} -- ${r.entryInfo.pubDate}") )
    val fragmentTexts = resolveds.map(resolved => renderSingleFragment(renderLocation, resolved, Entry.Presentation.Multiple))
    val unifiedFragmentTexts = fragmentTexts.mkString(entrySeparator)
    val fullText = renderMultiplePrologue + unifiedFragmentTexts + renderMultipleEpilogue
    val layoutPageInput = Layout.Input.Page(this, site, renderLocation, fullText, resolveds)
    layoutPage( layoutPageInput )

  def renderRange(renderLocation: SiteLocation, from: Instant, until: Instant): String =
    val ordering = summon[Ordering[Instant]]
    val rs = entriesResolved.filter(r => ordering.gteq(from, r.entryInfo.pubDate) && ordering.lt(r.entryInfo.pubDate, until))
    renderMultiple(renderLocation, rs.toVector)

  def renderSince(renderLocation: SiteLocation, from: Instant): String =
    renderRange( renderLocation, from, Instant.now)

  override def endpointBindings : immutable.Seq[ZTEndpointBinding] =
    val basicBindings = super.endpointBindings
    val mainRssBinding = ZTEndpointBinding.publicReadOnlyRss( rssFeed, zio.ZIO.attempt( feedBytes ), rssFeedIdentifiers )
    val pastRevisionBindings =
      revisionBinder.fold(List.empty[ZTEndpointBinding]): rb =>
        entriesResolved.toList
          .flatMap( er => er.entryInfo.updateHistory.toList.map( ur => (er.entryInfo.permalinkPathSiteRooted, ur ) ) )
          .collect { case ( ppsr, UpdateRecord(_, _, Some( revisionSpec ) ) ) => rb.revisionEndpointBinding(revisionSpec,ppsr,"text/html", immutable.Set.empty) }
    (basicBindings ++ pastRevisionBindings) :+ mainRssBinding

    val stage0 = ZTEndpointBinding.Source.Trivial( (basicBindings ++ pastRevisionBindings) :+ mainRssBinding )

    val diffPrerequesites : Option[Tuple2[RevisionBinder.RevisionPathFinder,DiffBinder]] =
      for
        _revisionBinder <- this.revisionBinder
        _diffBinder <- this.diffBinder
      yield
        Tuple2( _revisionBinder.revisionPathFinder, _diffBinder )

    val withDiffs =
      diffPrerequesites match
        case Some( Tuple2( rpf, db ) ) =>
          val diffTuples =
            entriesResolved.toList.flatMap: entry =>
              val noncurrent =
                nonCurrentUpdateRecordToOwnLatestMinorRevisionSpec(entry.entryInfo.updateHistory).toList
                  .collect { case Tuple2( UpdateRecord(_, _, Some(srs)), lmrs ) => ( entry.entryInfo.permalinkPathSiteRooted, srs, Some(lmrs) ) }
              val current = entry.entryInfo.updateHistory.headOption.flatMap( _.supercededRevisionSpec ).map( srs => (entry.entryInfo.permalinkPathSiteRooted, srs, None) )
              noncurrent ++ current
          val sourceBindingBySiteRootedPath : Rooted => ZTEndpointBinding = rooted => stage0.bindingBySiteRootedPath(rooted) 
          val diffBindings =
            diffTuples.map: tup =>
              val ( origSiteRootedPath, before, after ) = tup
              db.diffEndpointBinding( sourceBindingBySiteRootedPath, rpf, origSiteRootedPath, before, after, Set.empty )
          stage0.endpointBindings ++ diffBindings    
        case None =>
          stage0.endpointBindings
          
    withDiffs
