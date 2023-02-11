package unstatic.ztapir

import scala.collection.*
import unstatic.*

import zio.*

trait ZTBlog extends Blog with ZTEndpointBinding.Source:
  type Site <: ZTSite

  // you can override this
  val frontPageIdentifiers : immutable.Set[String] = immutable.Set("frontPage")

  def endpointBindings : immutable.Seq[ZTEndpointBinding] =
    val entryPageBindings =
      entriesResolved.to(Vector).map { resolved =>
        val pl = permalink( resolved )
        ZTEndpointBinding.publicReadOnlyHtml(pl, ZIO.attempt( renderSingle(pl,resolved )), fqnSuffixes(resolved.entryUntemplate).toSet)
      }.toVector
    val frontPageBinding =
      val maxToRender = maxFrontPageEntries.getOrElse( Int.MaxValue )
      ZTEndpointBinding.publicReadOnlyHtml(frontPage, ZIO.attempt( renderTop( frontPage, maxToRender ) ), frontPageIdentifiers )
    entryPageBindings :+ frontPageBinding


