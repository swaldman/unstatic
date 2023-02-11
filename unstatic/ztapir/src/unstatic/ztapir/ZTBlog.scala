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

  def endpointBindings : immutable.Seq[ZTEndpointBinding] =
    val entryPageBindings =
      entriesResolved.to(Vector).map { resolved =>
        val pl = permalink( resolved )
        site.publicReadOnlyHtml(pl, ZIO.attempt( renderSingle(pl,resolved )), fqnSuffixes(resolved.entryUntemplate).toSet, resolveHashSpecials, memoize)
      }.toVector
    val frontPageBinding =
      val maxToRender = maxFrontPageEntries.getOrElse( Int.MaxValue )
      site.publicReadOnlyHtml(frontPage, ZIO.attempt( renderTop( frontPage, maxToRender ) ), frontPageIdentifiers, resolveHashSpecials, memoize)
    entryPageBindings :+ frontPageBinding


