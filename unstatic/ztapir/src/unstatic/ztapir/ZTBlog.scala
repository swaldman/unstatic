package unstatic.ztapir

import scala.collection.*
import unstatic.*

import zio.*

trait ZTBlog extends Blog with ZTEndpointBinding.Source:
  type Site <: ZTSite

  def endpointBindings : immutable.Seq[ZTEndpointBinding] =
    val entryPageBindings =
      entriesResolved.map { resolved =>
        val pl = permalink( resolved )
        ZTEndpointBinding.publicReadOnlyHtml(pl, ZIO.attempt( renderSingle(pl,resolved )))
      }.toVector
    val frontPageBinding =
      ZTEndpointBinding.publicReadOnlyHtml(frontPage, ZIO.attempt( renderTop( frontPage, maxFrontPageEntries ) ) )
    entryPageBindings :+ frontPageBinding


