package unstatic.ztapir

import scala.collection.*
import java.time.Instant
import java.nio.file.Path as JPath
import unstatic.UrlPath.*
import unstatic.*

// TODO: Generalize Entry.Info type
//       Build a Warned monad that collects warnings along the path
trait ZTBlog[S <: ZTSite, M] extends ZTEndpointBinding.Source:
  object Entry:
    final case class Info (
      mbTitle : Option[String],
      authors : Seq[String],
      tags : Seq[String],
      pubDate : Instant,
      contentType : String,
      mediaPath : Rooted, // from Site root
      permalinkSiteRootedPath : Rooted // from SiteRoot
    )
    object Resolved:
      given Ordering[Resolved] = Ordering.by( (r : Resolved) => (r.info.pubDate, r.untemplate.UntemplatePackage, r.untemplate.UntemplateName) ).reverse
    final case class Resolved( untemplate : ZTBlog.this.Untemplate, info : Entry.Info )
  final case class Entry(mediaPathSiteRooted : Rooted, presentationMultiple : Boolean, site : S)
  type Untemplate = untemplate.Untemplate[Entry,M]
  val site : S
  def untemplates                                                               : immutable.Vector[Untemplate]
  def entryInfo( template : Untemplate )                                        : Entry.Info
  def renderSingle( template : Entry.Resolved, presentationMultiple : Boolean ) : String
  def renderLast( num : Int )                                                   : String
  def renderRange( from : Instant, until : Instant ) : String

  def renderSince( moment : Instant ) : String = renderRange( moment, Instant.now )

  def endpointBindings : immutable.Seq[ZTEndpointBinding]
