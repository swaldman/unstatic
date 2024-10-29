package unstatic.ztapir.simple

import scala.collection.immutable
import scala.util.Try
import java.time.*
import java.time.format.DateTimeFormatter.{ISO_INSTANT, ISO_LOCAL_DATE}
import java.time.temporal.ChronoField

import unstatic.*
import unstatic.UrlPath.*

import org.jsoup.Jsoup
import org.jsoup.nodes.Document as JsoupDocument

import Attribute.Key
import sttp.tapir.EndpointIO.annotations.description
import unstatic.ztapir.ZTSite

object UpdateRecord:
  def apply( timestamp : String, description : Option[String], supercededVersionSpec : Option[String], revisionAuthors : Option[Seq[String]] = None ) : UpdateRecord =
    apply( parseTimestamp(timestamp).get, description, supercededVersionSpec, revisionAuthors )

  case class ForDisplay(
    timestamp : Instant,
    description : Option[String],
    finalMinorRevisionSpec : Option[String],
    supercededRevisionSpec : Option[String],
    finalMinorRevisionRelative : Option[Rel],
    supercededRevisionRelative : Option[Rel],
    diffRelative : Option[Rel],
    revisionAuthors : Option[Seq[String]]
  )

  given ordering : Ordering[UpdateRecord] =
    Ordering.by( (ur : UpdateRecord) => (ur.timestamp,ur.description,ur.supercededRevisionSpec, ur.revisionAuthors.map(_.mkString)) ).reverse

case class UpdateRecord( timestamp : Instant, description : Option[String], supercededRevisionSpec : Option[String], revisionAuthors : Option[Seq[String]] )

object Related:
  case class Multi( relateds : immutable.Seq[Related], title : Option[String] )
  case class Item( linkUrlOrUid : String, author : Option[String], title : Option[String] )
case class Related( base : String = "Related", category : Option[String], items : immutable.Seq[Related.Item] )

private val LINESEP = scala.util.Properties.lineSeparator

def defaultRenderRelatedList( site : ZTSite, renderedOnSiteRootedPath : Rooted, related : Related, relatedListClass : String = "related-list" ) : String =
  def defaultRenderItem( item : Related.Item ) : String =
    val (resolvedLinkUrl, absoluteUrl) =
      val fromId =
        site.siteRootedPathByUniqueIdentifier( item.linkUrlOrUid.dropWhile( _ == '#' ) ).map( srp => (srp, renderedOnSiteRootedPath.relativizeSibling(srp)) )
      fromId match
        case Some( siteRootedUrl, relativeUrl ) => (relativeUrl.toString(), site.absFromSiteRooted(siteRootedUrl).toString)
        case None                => (item.linkUrlOrUid, item.linkUrlOrUid)
    val text =
      ( item.author, item.title ) match
        case ( Some(author), Some(title) ) => s"${author} &mdash; ${title}"
        case ( Some(author), None        ) => author
        case ( None,         Some(title) ) => title
        case ( None,         None        ) => absoluteUrl
    s"""  <li><a href="${resolvedLinkUrl}">${text}</a></li>"""
  s"""<ol class="${relatedListClass}">""" + LINESEP + related.items.map( defaultRenderItem ).mkString( LINESEP ) + LINESEP + "</ol>"

def defaultRenderRelated(
  site                     : ZTSite,
  renderedOnSiteRootedPath : Rooted,
  related                  : Related,
  relatedDivClass          : String = "related",
  relatedTitleClass        : String = "related-title",
  relatedListClass         : String = "related-list"
) : String =
  val renderRelatedTitle = related.category.fold(related.base)(c => s"${related.base} &mdash; ${c}")
  val sb = new java.lang.StringBuilder
  sb.append(s"""<div class="${relatedDivClass}">""")
  sb.append(LINESEP)
  sb.append(s"""  <div class="${relatedTitleClass}">""")
  sb.append(LINESEP)
  sb.append(defaultRenderRelatedList(site,renderedOnSiteRootedPath,related, relatedListClass))
  sb.append(LINESEP)
  sb.append( """  </div>""")
  sb.append(LINESEP)
  sb.append( """</div>""")
  sb.toString

def defaultRenderRelatedMulti(
  site                     : ZTSite,
  renderedOnSiteRootedPath : Rooted,
  relatedMulti             : Related.Multi,
  relatedMultiClass        : String = "related-multi",
  relatedMultiTitleClass   : String = "related-multi-title",
  relatedDivClass          : String = "related",
  relatedTitleClass        : String = "related-title",
  relatedListClass         : String = "related-list"
) : String =
  val sb = new java.lang.StringBuilder
  sb.append(s"""<div class="${relatedMultiClass}">""")
  sb.append(LINESEP)
  relatedMulti.title.foreach( title => sb.append(s"""  <div class="${relatedMultiTitleClass}">${title}</div>""") )
  sb.append(LINESEP)
  relatedMulti.relateds.foreach(related => defaultRenderRelated(site, renderedOnSiteRootedPath, related, relatedDivClass, relatedTitleClass, relatedListClass))
  sb.append("</div>")
  sb.toString

def defaultRenderRelatedOrMulti(
  site                     : ZTSite,
  renderedOnSiteRootedPath : Rooted,
  relatedOrMulti           : Related | Related.Multi,
  relatedMultiClass        : String = "related-multi",
  relatedMultiTitleClass   : String = "related-multi-title",
  relatedDivClass          : String = "related",
  relatedTitleClass        : String = "related-title",
  relatedListClass         : String = "related-list"
) : String =
  relatedOrMulti match
    case related : Related       => defaultRenderRelated(site,renderedOnSiteRootedPath,related,relatedDivClass,relatedTitleClass,relatedListClass)
    case multi   : Related.Multi => defaultRenderRelatedMulti(site,renderedOnSiteRootedPath,multi,relatedMultiClass,relatedMultiTitleClass,relatedDivClass,relatedTitleClass,relatedListClass)

def findContentType( ut : untemplate.Untemplate[?,?] ) : String =
  (Key.`Content-Type`.caseInsensitiveCheck(ut) orElse contentTypeFromSuffix(ut.UntemplateName)).getOrElse("text/plain")

def normalizeContentType( contentType : String ) =
  val semi = contentType.indexOf(';')
  if semi < 0 then
    contentType
  else
    // XXX: Log something about ignoring params
    contentType.substring(0,semi)

def missingAttribute( ut : untemplate.Untemplate[?,?], key : Attribute.Key[?] ) : unstatic.MissingAttribute =
  new MissingAttribute(s"${ut} is missing required attribute '${key}'.")

def parseTimestamp(timestamp: String, timeZone : ZoneId = ZoneId.systemDefault()): Try[Instant] =
  parseTimestampIsoInstant(timestamp) orElse parseTimestampFromIsoLocalDate(timestamp, timeZone)

def contentTypeFromSuffix( name : String ) : Option[String] =
  val suffixDelimiter = name.lastIndexOf("_")
  if suffixDelimiter >= 0 then
    val suffix = name.substring(suffixDelimiter + 1)
    ContentTypeBySuffix.get(suffix)
  else
    // XXX: we should log something in this case
    None

private val ContentTypeBySuffix = immutable.Map (
  "html" -> "text/html",
  "md"   -> "text/markdown",
  "txt"  -> "text/plain",
)

// expects a (mutable) Jsoup Document parsed with a base URL!
// thanks https://stackoverflow.com/a/26956350/1413240
private def mutateResolveRelativeUrls( doc : JsoupDocument ) : Unit =
  import scala.jdk.CollectionConverters._
  def absolutize(cssQuery : String, refAttr : String) =
    doc.select(cssQuery).asScala.foreach( elem => elem.attr(refAttr, elem.absUrl(refAttr)))
  absolutize("a", "href")
  absolutize("img","src")

private def parseTimestampIsoInstant(timestamp: String): Try[Instant] =
  for
    temporalAccessor <- Try(ISO_INSTANT.parse(timestamp))
  yield
    Instant.from(temporalAccessor)

private def parseTimestampFromIsoLocalDate(timestamp: String, timeZone : ZoneId = ZoneId.systemDefault()): Try[Instant] =
  for
    temporalAccessor <- Try(ISO_LOCAL_DATE.parse(timestamp))
    ld <- Try(LocalDate.from(temporalAccessor))
    zdt <- Try(ld.atTime(12, 0).atZone(timeZone))
  yield
    Instant.from(zdt)

