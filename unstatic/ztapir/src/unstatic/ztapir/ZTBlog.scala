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
  val site : S
  type SiteLocation = site.SiteLocation
  type Untemplate = untemplate.Untemplate[Entry,M]
  def untemplates                        : immutable.Vector[Untemplate]
  def entryInfo( template : Untemplate ) : Entry.Info
  def renderSingle( renderLocation : SiteLocation, template : Entry.Resolved, presentationMultiple : Boolean ) : String
  def renderLast( renderLocation : SiteLocation, num : Int )                                                   : String
  def renderRange( renderLocation : SiteLocation, from : Instant, until : Instant )                            : String

  def renderSince( renderLocation : SiteLocation, moment : Instant ) : String = renderRange( renderLocation, moment, Instant.now )

  def endpointBindings : immutable.Seq[ZTEndpointBinding]
