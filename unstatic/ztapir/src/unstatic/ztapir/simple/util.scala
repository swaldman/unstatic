package unstatic.ztapir.simple

import scala.collection.*
import scala.util.Try
import java.time.*
import java.time.format.DateTimeFormatter.{ISO_INSTANT, ISO_LOCAL_DATE}
import java.time.temporal.ChronoField

import unstatic.*
import unstatic.UrlPath.*

import org.jsoup.Jsoup
import org.jsoup.nodes.Document as JsoupDocument

import Attribute.Key

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

def parseTimestamp(timestamp: String): Try[Instant] =
  parseTimestampIsoInstant(timestamp) orElse parseTimestampFromIsoLocalDate(timestamp)

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
// return the modified document for a fluent API style
// thanks https://stackoverflow.com/a/26956350/1413240
private def resolveRelativeUrls( doc : JsoupDocument ) : JsoupDocument =
  import scala.jdk.CollectionConverters._
  def absolutize(cssQuery : String, refAttr : String) =
    doc.select(cssQuery).asScala.foreach( elem => elem.attr(refAttr, elem.absUrl(refAttr)))
  absolutize("a", "href")
  absolutize("img","src")
  doc



private def parseTimestampIsoInstant(timestamp: String): Try[Instant] =
  for
    temporalAccessor <- Try(ISO_INSTANT.parse(timestamp))
  yield
    Instant.from(temporalAccessor)

private def parseTimestampFromIsoLocalDate(timestamp: String): Try[Instant] =
  for
    temporalAccessor <- Try(ISO_LOCAL_DATE.parse(timestamp))
    ld <- Try(LocalDate.from(temporalAccessor))
    zdt <- Try(ld.atTime(12, 0).atZone(ZoneId.systemDefault()))
  yield
    Instant.from(zdt)

