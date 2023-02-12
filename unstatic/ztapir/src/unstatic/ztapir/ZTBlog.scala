package unstatic.ztapir

import scala.collection.*
import unstatic.*

import zio.*

trait ZTBlog extends Blog with ZTEndpointBinding.Source:
  type Site <: ZTSite

  private val DefaultFrontPageIdentifiers = immutable.Set("blogFrontPage")

  // you can override this
  def frontPageIdentifiers : immutable.Set[String] = DefaultFrontPageIdentifiers

  // you can override this
  val resolveHashSpecials = true

  // you can override this
  val memoize = true

  def mediaDir( resolved : EntryResolved ) : SiteLocation

  def endpointBindings : immutable.Seq[ZTEndpointBinding] =
    val entryPageBindings =
      entriesResolved.to(Vector).map { resolved =>
        val pl = permalink( resolved )
        val md = mediaDir( resolved )
        site.publicReadOnlyHtml(pl, ZIO.attempt( renderSingle(pl,resolved )), Some(md.siteRootedPath), fqnSuffixes(resolved.entryUntemplate).toSet, resolveHashSpecials, memoize)
      }.toVector
    val frontPageBinding =
      val maxToRender = maxFrontPageEntries.getOrElse( Int.MaxValue )
      site.publicReadOnlyHtml(frontPage, ZIO.attempt( renderTop( frontPage, maxToRender ) ), None, frontPageIdentifiers, resolveHashSpecials, memoize)
    entryPageBindings :+ frontPageBinding


