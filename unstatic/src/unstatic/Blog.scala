package unstatic

import scala.collection.*

trait Blog:
  type Site <: unstatic.Site
  type EntryInfo
  type EntryInput
  type EntryMetadata
  type EntryUntemplate = untemplate.Untemplate[EntryInput,EntryMetadata]

  case class EntryResolved( entryInfo : EntryInfo, entryUntemplate : EntryUntemplate )

  /**
   * Usually reverse-chronological!
   */
  given entryOrdering : Ordering[EntryResolved]

  val site : Site

  type     SiteLocation = site.SiteLocation
  lazy val SiteLocation = site.SiteLocation // better be lazy or we'll fail on construction!

  val frontPage           : SiteLocation
  val maxFrontPageEntries : Int

  def narrowRawUntemplate( rawUntemplate : untemplate.Untemplate[Any,Nothing] ) =
    rawUntemplate.asInstanceOf[untemplate.Untemplate[EntryInput,EntryMetadata]]

  def entryUntemplates : immutable.Set[EntryUntemplate]

  def entryInfo( template : EntryUntemplate ) : EntryInfo

  lazy val entriesResolved : immutable.SortedSet[EntryResolved] =
    entryUntemplates.map(ut => EntryResolved(entryInfo(ut), ut)).to(immutable.SortedSet)

  def entryInput( renderLocation : SiteLocation, resolved : EntryResolved, presentationMultiple : Boolean ) : EntryInput

  def permalink( resolved : EntryResolved ) : SiteLocation

  def renderSingle( renderLocation : SiteLocation, resolved : EntryResolved ) : String

  def renderMultiple( renderLocation : SiteLocation, resolveds : immutable.Seq[EntryResolved] ) : String

  def renderTop( renderLocation : SiteLocation, n : Int ) : String = renderMultiple( renderLocation, entriesResolved.take(n).to(Vector) )
