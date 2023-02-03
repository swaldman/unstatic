package unstatic

import scala.collection.*

trait Blog:
  type Site <: unstatic.Site
  type EntryInfo
  type EntryInput
  type EntryMetadata
  type EntryUntemplate = untemplate.Untemplate[EntryInput,EntryMetadata]

  case class EntryResolved( entryInfo : EntryInfo, entryUntemplate : EntryUntemplate ):
    def permalink = Blog.this.permalink( this )

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

  def entryId( entryResolved : EntryResolved ) : String

  lazy val mapEntryById = entriesResolved.map( resolved => Tuple2(entryId(resolved), resolved) ).toMap

  def entryById( id : String ) : EntryResolved =
    mapEntryById.get(id) match {
      case Some( resolved ) => resolved
      case None             => throw new NoSuchEntry(s"Cannot find '${id}' among entry IDs " + mapEntryById.map( _(0) ).mkString(", "))
    }

  def permalink( resolved : EntryResolved ) : SiteLocation

  def renderSingle( renderLocation : SiteLocation, resolved : EntryResolved ) : String

  def renderMultiple( renderLocation : SiteLocation, resolveds : immutable.Seq[EntryResolved] ) : String

  def renderTop( renderLocation : SiteLocation, n : Int ) : String = renderMultiple( renderLocation, entriesResolved.take(n).to(Vector) )
