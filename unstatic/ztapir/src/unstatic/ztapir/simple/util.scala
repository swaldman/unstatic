package unstatic.ztapir.simple

import scala.collection.*
import scala.util.Try
import java.time.*
import java.time.format.DateTimeFormatter.{ISO_INSTANT, ISO_LOCAL_DATE}
import java.time.temporal.ChronoField

import unstatic.*
import unstatic.UrlPath.*

def findContentType( checkable : Attribute.Checkable, ut : untemplate.Untemplate[?,?] ) : String =
  import Attribute.Key.*
  (checkable.check(`Content-Type`) orElse contentTypeFromSuffix(ut.UntemplateName)).getOrElse("text/plain")

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
    None

private val ContentTypeBySuffix = immutable.Map (
  "html" -> "text/html",
  "md"   -> "text/markdown",
  "txt"  -> "text/plain",
)

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

