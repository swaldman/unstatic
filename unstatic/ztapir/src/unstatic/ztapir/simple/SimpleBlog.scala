package unstatic.ztapir.simple

import java.time.Instant

import scala.collection.*

import unstatic.*
import unstatic.UrlPath.*
import unstatic.ztapir.*

object SimpleBlog:
  object Entry:
    final case class Info (
      mbTitle : Option[String],
      authors : Seq[String],
      tags : Seq[String],
      pubDate : Instant,
      contentType : String,
      mediaPathSiteRooted : Rooted, // from Site root
      permalinkPathSiteRooted : Rooted // from SiteRoot
    )
    final case class Input(renderPath : SimpleBlog#SiteLocation, mediaPath : SimpleBlog#SiteLocation, presentationMultiple : Boolean)
trait SimpleBlog extends ZTBlog:
  import SimpleBlog.*

  type EntryInfo      = Entry.Info
  type EntryInput     = Entry.Input
  type EntryMetadata  = Nothing

  /**
   * Usually reverse-chronological!
   */
  given entryOrdering : Ordering[EntryResolved] =
    Ordering.by( (er : EntryResolved) => (er.entryInfo.pubDate, er.entryUntemplate.UntemplatePackage, er.entryUntemplate.UntemplateName) ).reverse

  val site                : Site
  val frontPage           : SiteLocation
  val maxFrontPageEntries : Int

  def entryUntemplates : immutable.Set[EntryUntemplate]

  def entryInfo( template : EntryUntemplate ) : EntryInfo = ???

  def entryInput( renderLocation : SiteLocation, resolved : EntryResolved, presentationMultiple : Boolean ) : EntryInput

  def permalink( resolved : EntryResolved ) : SiteLocation

  def renderSingle( renderLocation : SiteLocation, resolved : EntryResolved ) : String

  def renderMultiple( renderLocation : SiteLocation, resolveds : immutable.Seq[EntryResolved] ) : String

  /*
    def entryInfo( untemplate : this.Untemplate ) =
      val attrsLc = untemplate.UntemplateAttributes.map { case (k, v) => (k.toLowerCase, v) }
      def getMaybeMultiple(keySingular : String) : Seq[String] =
        attrsLc.get(keySingular + "s") match
          case Some(seq: Seq[_]) => seq.map( _.toString ) // to avoid unchecked Seq[String] match
          case Some(str: String) => str.split(",").map(_.trim).toSeq
          case Some( other ) => throw new D24nSite.Exception(s"Unexpected '${keySingular}s' type: ${other}")
          case None =>
            attrsLc.get(keySingular) match
              case Some(str: String) => Seq(str)
              case Some( other ) => throw new D24nSite.Exception(s"Unexpected '${keySingular}' type: ${other}")
              case None => Nil



      val mbTitle = attrsLc.get("title").map( _.toString )
      val authors = getMaybeMultiple("author")
      val tags    = getMaybeMultiple("tag")
      val pubDate =
        attrsLc.get("pubdate") orElse attrsLc.get("publicationdate") match
          case Some( instant : Instant )  => instant
          case Some( timestamp : String ) => parseTimestamp( timestamp.trim )
          case Some( other )              => throw new D24nSite.Exception(s"Unexpected publication date format: ${other}")
          case None                       => throw new D24nSite.Exception(s"PubDate or PublicationDate attribute required, not found.")
      val contentType =
        (attrsLc.get("content-type").map( _.toString ) orElse contentTypeFromSuffix(untemplate.UntemplateName)).getOrElse("text/plain")
      val mbPermalink = attrsLc.get("permalink").map( _.toString )
      val mbLinkName = attrsLc.get("linkname").map( _.toString )
      val (mediaPath, permalinkSiteRootedPath) = mediaPathPermalink(mbPermalink, pubDate, mbTitle.getOrElse("Untitled Post"), mbLinkName)
      Entry.Info(mbTitle, authors, tags, pubDate, contentType, mediaPath, permalinkSiteRootedPath)
  */