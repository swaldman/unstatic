package unstatic

import UrlPath.*
import java.nio.file.Path as JPath
import scala.collection.*

object StaticLocationBinding:
  trait Source:
    def locationBindings : immutable.Seq[StaticLocationBinding]

  val IdentifierOrdering = AnyBinding.IdentifierOrdering

  def staticLocationBinding( siteRootedPath : Rooted, source : JPath, identifiers : immutable.Set[String] = immutable.Set.empty ) : StaticLocationBinding =
    StaticLocationBinding( siteRootedPath, source, immutable.SortedSet.from(identifiers)(using IdentifierOrdering))
case class StaticLocationBinding private[StaticLocationBinding]( siteRootedPath : Rooted, source : JPath, identifiers : immutable.SortedSet[String] ) extends AnyBinding
