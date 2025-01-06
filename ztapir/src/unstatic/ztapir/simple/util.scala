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

case class SingleItemRssSpec( siteRooted : Rooted, title : Option[String] ):
  def htmlLinkAlternateRelative( fromSiteRooted : Rooted ) =
    val titlePart =
      title match
        case Some( t ) =>
          val safeTitle = t.replaceAll("\"","&quot;").replaceAll(">","&gt;").replaceAll("<","&lt;")
          s"""title="${safeTitle}" """
        case None =>
          " "
    s"""<link rel="alternate" type="application/x-single-item-rss+xml" ${titlePart}href="${fromSiteRooted.relativizeSibling(siteRooted)}">"""

private val LINESEP = scala.util.Properties.lineSeparator

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

