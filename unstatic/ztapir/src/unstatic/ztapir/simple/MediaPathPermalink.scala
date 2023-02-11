package unstatic.ztapir.simple

import scala.collection.*
import scala.util.Try
import java.time.*
import java.time.format.DateTimeFormatter.{ISO_INSTANT, ISO_LOCAL_DATE}
import java.time.temporal.ChronoField

import unstatic.*
import unstatic.UrlPath.*

import untemplate.Untemplate.AnyUntemplate

object MediaPathPermalink:
  import Attribute.Key

  type Generator = (AnyUntemplate) => MediaPathPermalink

  def fullyQualifiedNameDir( ut : AnyUntemplate ) : MediaPathPermalink =
    val dirSitePath = ut.UntemplateFullyQualifiedName.map( c => if c == '.' then '/' else c )
    val mediaPath = Rooted.parseAndRoot(dirSitePath)
    MediaPathPermalink( mediaPath, mediaPath.resolve("index.html") )

  def yearMonthDayNameDir( ut : AnyUntemplate ) : MediaPathPermalink =
    val pubDate = Key.`PubDate`.caseInsensitiveCheck(ut).getOrElse {
      throw missingAttribute(ut, Key.`PubDate`)
    }
    val linkName =
      (Key.`LinkName`.caseInsensitiveCheck(ut) orElse Key.`Title`.caseInsensitiveCheck(ut).map(linkableTitle)).getOrElse(ut.UntemplateName)
    ensureNoFilePathChars(linkName)
    yearMonthDayNameDir(pubDate, linkName)

  def givenPermalinkInMediaDirOrElse( backstop : Generator ) : Generator = { ut =>
    mbGivenPermalinkInMediaDir(ut).getOrElse( backstop(ut) )
  }

  def givenPermalinkInMediaDir( ut : AnyUntemplate ) : MediaPathPermalink =
    mbGivenPermalinkInMediaDir( ut ).getOrElse {
      throw missingAttribute(ut, Key.`Permalink`)
    }

  def mbGivenPermalinkInMediaDir( ut : AnyUntemplate ) : Option[MediaPathPermalink] =
    Key.`Permalink`.caseInsensitiveCheck(ut).map(givenPermalinkInMediaDir)

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
