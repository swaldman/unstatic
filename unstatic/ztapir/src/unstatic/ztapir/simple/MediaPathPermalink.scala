package unstatic.ztapir.simple

import scala.collection.*
import scala.util.Try
import java.time.*
import java.time.format.DateTimeFormatter.{ISO_INSTANT, ISO_LOCAL_DATE}
import java.time.temporal.ChronoField

import unstatic.*
import unstatic.UrlPath.*

object MediaPathPermalink:
  import Attribute.Key.*

  def fullyQualifiedNameDir( checkable : Attribute.Checkable, ut : untemplate.Untemplate[?,?] ) : MediaPathPermalink =
    val dirSitePath = ut.UntemplateFullyQualifiedName.map( c => if c == '.' then '/' else c )
    val mediaPath = Rooted.parseAndRoot(dirSitePath)
    MediaPathPermalink( mediaPath, mediaPath.resolve("index.html") )

  def yearMonthDayNameDir( checkable : Attribute.Checkable, ut : untemplate.Untemplate[?,?] ) : MediaPathPermalink =
    val pubDate = checkable.check(`PubDate`).getOrElse {
      throw missingAttribute(ut, `PubDate`)
    }
    val linkName = (checkable.check(`LinkName`) orElse checkable.check(`Title`).map(linkableTitle)).getOrElse(ut.UntemplateName)
    ensureNoFilePathChars(linkName)
    yearMonthDayNameDir(pubDate, linkName)

  def givenPermalinkInMediaDir( checkable : Attribute.Checkable, ut : untemplate.Untemplate[?,?] ) : MediaPathPermalink =
    val permalink = checkable.check(`Permalink`).getOrElse {
      throw missingAttribute(ut, `Permalink`)
    }
    givenPermalinkInMediaDir( permalink )

  def givenMediaDirAndPermalink(mediaDirSiteRooted : String, permalinkSiteRooted: String): MediaPathPermalink =
    MediaPathPermalink(Rooted.parseAndRoot(mediaDirSiteRooted), Rooted.parseAndRoot(permalinkSiteRooted))

  private def yearMonthDayNameDir(pubDate : Instant, computedLinkName : String ) : MediaPathPermalink =
    val zoned = pubDate.atZone(ZoneId.systemDefault())
    val year  = zoned.get(ChronoField.YEAR)
    val month = zoned.get(ChronoField.MONTH_OF_YEAR)
    val day   = zoned.get(ChronoField.DAY_OF_MONTH)
    val mediaPath = f"/$year%d/$month%02d/$day%02d/${computedLinkName}%s/"
    MediaPathPermalink( Rooted(mediaPath), Rooted(mediaPath + "index.html") )

  private def givenPermalinkInMediaDir(permalinkSiteRooted : String) : MediaPathPermalink =
    val pl = Rooted.parseAndRoot(permalinkSiteRooted)
    MediaPathPermalink( pl.parent, pl )

case class MediaPathPermalink( mediaPathSiteRooted : Rooted, permalinkSiteRooted : Rooted )
