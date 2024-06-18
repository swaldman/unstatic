package unstatic

import scala.collection.*

object Blog:
  enum EntryPresentation:
    case Single, Multiple, Rss
trait Blog:
  import Blog.EntryPresentation

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
  val maxFrontPageEntries : Option[Int]

  def narrowRawUntemplate( rawUntemplate : untemplate.Untemplate[Any,Nothing] ) =
    rawUntemplate.asInstanceOf[untemplate.Untemplate[EntryInput,EntryMetadata]]

  def entryUntemplates : immutable.Set[EntryUntemplate]

  /**
    * Override to augment basic entries with synthetic untemplates.
    */
  def syntheticEntryUntemplates( organicEntryUntemplates : immutable.Set[EntryUntemplate] ) : immutable.Set[EntryUntemplate] = immutable.Set.empty

  def allEntryUntemplates : immutable.Set[EntryUntemplate] =
    val organic = entryUntemplates
    val synthetic = syntheticEntryUntemplates(organic)
    organic ++ synthetic

  def entryInfo( template : EntryUntemplate ) : EntryInfo

  lazy val entriesResolved : immutable.SortedSet[EntryResolved] =
    allEntryUntemplates.map(ut => EntryResolved(entryInfo(ut), ut)).to(immutable.SortedSet)

  def entryInput( renderLocation : SiteLocation, resolved : EntryResolved, presentation : EntryPresentation ) : EntryInput

  def entryIds( entryResolved : EntryResolved ) : List[String] = fqnSuffixes(entryResolved.entryUntemplate)

  lazy val mapEntryById =
    val rawTuples = entriesResolved.toList.flatMap( resolved => entryIds(resolved).map( id => Tuple2(id, resolved) ) )
    val ambiguousKeys = rawTuples.groupBy( _(0) ).filter{ case (id, matchTups) => matchTups.length > 1 }.keys.toSet
    if ambiguousKeys.nonEmpty then
      // XXX: we need to choose a logging scheme!
      println(s"The following potential entry IDs could refer to multiple entries, so cannot be used: " + ambiguousKeys.mkString(", "))
    rawTuples.filter( tup => !ambiguousKeys(tup(0)) ).toMap

  def entryById( id : String ) : EntryResolved =
    mapEntryById.get(id) match {
      case Some( resolved ) => resolved
      case None             => throw new NoSuchEntry(s"Cannot find '${id}' among entry IDs " + mapEntryById.map( _(0) ).mkString(", "))
    }

  def permalink( resolved : EntryResolved ) : SiteLocation

  def renderSingle( renderLocation : SiteLocation, resolved : EntryResolved ) : String

  def renderMultiple( renderLocation : SiteLocation, resolveds : immutable.Seq[EntryResolved] ) : String

  def renderTop( renderLocation : SiteLocation, n : Int ) : String = renderMultiple( renderLocation, entriesResolved.take(n).to(Vector) )
