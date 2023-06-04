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

  type Source = (AnyUntemplate) => MediaPathPermalink

  def overridable( source : Source, ut : AnyUntemplate ) : MediaPathPermalink =
    overridable( source )( ut )

  def overridable( source : Source ) : Source = { (ut : AnyUntemplate) =>
    val overrideMediaDir  = Key.`MediaDir`.caseInsensitiveCheck(ut).map( Rooted.parseAndRoot )
    val overridePermalink = Key.`Permalink`.caseInsensitiveCheck(ut).map( Rooted.parseAndRoot )
    ( overrideMediaDir, overridePermalink ) match
      case (Some(omd), Some(op)) => MediaPathPermalink(omd, op)
      case (Some(omd), None    ) => MediaPathPermalink(omd, source(ut).permalinkSiteRooted)
      case (None     , Some(op)) => MediaPathPermalink(source(ut).mediaPathSiteRooted, op)
      case (None     , None    ) => source(ut)
  }

  def fullyQualifiedNameDir( ut : AnyUntemplate ) : MediaPathPermalink =
    val dirSitePath = ut.UntemplateFullyQualifiedName.map( c => if c == '.' then '/' else c )
    val mediaPath = Rooted.parseAndRoot(dirSitePath)
    MediaPathPermalink( mediaPath, mediaPath.resolve("index.html") )

  def yearMonthDayNameDir( timeZone : ZoneId )( ut : AnyUntemplate ) : MediaPathPermalink =
    val pubDate = Key.`PubDate`.caseInsensitiveCheck(ut).getOrElse {
      throw missingAttribute(ut, Key.`PubDate`)
    }
    val linkName =
      (Key.`LinkName`.caseInsensitiveCheck(ut) orElse Key.`Title`.caseInsensitiveCheck(ut).map(linkableTitle)).getOrElse(ut.UntemplateName)
    ensureNoFilePathChars(linkName)
    yearMonthDayNameDir(pubDate, linkName, timeZone)

  def givenPermalinkInMediaDirOrElse( backstop : Source ) : Source = { ut =>
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

  private def yearMonthDayNameDir(pubDate : Instant, computedLinkName : String, timeZone : ZoneId = ZoneId.systemDefault()) : MediaPathPermalink =
    val zoned = pubDate.atZone(timeZone)
    val year  = zoned.get(ChronoField.YEAR)
    val month = zoned.get(ChronoField.MONTH_OF_YEAR)
    val day   = zoned.get(ChronoField.DAY_OF_MONTH)
    val mediaPath = f"/$year%d/$month%02d/$day%02d/${computedLinkName}%s/"
    MediaPathPermalink( Rooted(mediaPath), Rooted(mediaPath + "index.html") )

  private def givenPermalinkInMediaDir(permalinkSiteRooted : String) : MediaPathPermalink =
    val pl = Rooted.parseAndRoot(permalinkSiteRooted)
    MediaPathPermalink( pl.parent, pl )

case class MediaPathPermalink( mediaPathSiteRooted : Rooted, permalinkSiteRooted : Rooted )
