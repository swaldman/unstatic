package unstatic.ztapir.simple

import scala.collection.*
import scala.util.Try
import java.time.*
import java.time.format.DateTimeFormatter.{ISO_INSTANT, ISO_LOCAL_DATE}
import java.time.temporal.ChronoField

import unstatic.*
import unstatic.UrlPath.*

def parseTimestamp(timestamp: String): Try[Instant] =
  parseTimestampIsoInstant(timestamp) orElse parseTimestampFromIsoLocalDate(timestamp)

def contentTypeFromSuffix( name : String ) : Option[String] =
  val suffixDelimiter = name.lastIndexOf("_")
  if suffixDelimiter >= 0 then
    val suffix = name.substring(suffixDelimiter + 1)
    ContentTypeBySuffix.get(suffix)
  else
    None

object MediaPathPermalink:
  def yearMonthDayName(pubDate : Instant, title : String, mbLinkName : Option[String] ) : MediaPathPermalink =
    val zoned = pubDate.atZone(ZoneId.systemDefault())
    val year  = zoned.get(ChronoField.YEAR)
    val month = zoned.get(ChronoField.MONTH_OF_YEAR)
    val day   = zoned.get(ChronoField.DAY_OF_MONTH)
    val linkName = mbLinkName.getOrElse(linkableTitle(title))
    ensureNoFilePathChars(linkName)
    val mediaPath = f"/$year%d/$month%02d/$day%02d/${linkName}%s/"
    MediaPathPermalink( Rooted(mediaPath), Rooted(mediaPath + "index.html") )

  def givenPermalinkInMediaDir(permalinkSiteRooted : String) : MediaPathPermalink =
    val pl = Rooted.parseAndRoot(permalinkSiteRooted)
    MediaPathPermalink( pl.parent, pl )

  def givenMediaDirAndPermalink(mediaDirSiteRooted : String, permalinkSiteRooted: String): MediaPathPermalink =
    MediaPathPermalink(Rooted.parseAndRoot(mediaDirSiteRooted), Rooted.parseAndRoot(permalinkSiteRooted))
case class MediaPathPermalink( mediaPathSiteRooted : Rooted, permalinkSiteRooted : Rooted )

// things that render fragments to output, usually HTML
type ContentRenderer =
  Function1[untemplate.Result[Nothing], untemplate.Result[Nothing]]

val ContentTypeBySuffix = immutable.Map (
  "html" -> "text/html",
  "md"   -> "text/markdown",
  "txt"  -> "text/plain",
)

val ContentRendererForContentType = immutable.Map[String,ContentRenderer] (
  "text/html" -> identity
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

