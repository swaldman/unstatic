package unstatic.ztapir

import scala.collection.*
import unstatic.*

import zio.*

trait ZTBlog extends Blog with ZTEndpointBinding.Source:
  type Site <: ZTSite

  private val DefaultFrontPageIdentifiers = immutable.Set("blogFrontPage")

  // you can override this
  def frontPageIdentifiers : immutable.Set[String] = DefaultFrontPageIdentifiers

  // you can override any of these
  val entryFragmentsResolveHashSpecials   = true   // memoization of fragments not supported
  val entryTopLevelResolveHashSpecials    = false
  val entryTopLevelMemoize                = true
  val multipleTopLevelResolveHashSpecials = false
  val multipleTopLevelMemoize             = true

  def mediaDir( resolved : EntryResolved ) : SiteLocation

  def identifiers( resolved : EntryResolved ) : immutable.Set[String] = fqnSuffixes(resolved.entryUntemplate).toSet

  def endpointBindings : immutable.Seq[ZTEndpointBinding] =
    val entryPageBindings =
      entriesResolved.to(Vector).map { resolved =>
        val pl  = permalink( resolved )
        val md  = mediaDir( resolved )
        val ids = identifiers( resolved )
        site.publicReadOnlyHtml(pl, ZIO.attempt( renderSingle(pl,resolved ) ), Some(md.siteRootedPath), ids, entryTopLevelResolveHashSpecials, entryTopLevelMemoize)
      }.toVector
    val frontPageBinding =
      val maxToRender = maxFrontPageEntries.getOrElse( Int.MaxValue )
      site.publicReadOnlyHtml(frontPage, ZIO.attempt( renderTop( frontPage, maxToRender ) ), None, frontPageIdentifiers, multipleTopLevelResolveHashSpecials, multipleTopLevelMemoize)
    entryPageBindings :+ frontPageBinding


