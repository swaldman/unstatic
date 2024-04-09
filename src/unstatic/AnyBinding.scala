package unstatic

import scala.collection.*
import UrlPath.Rooted

object AnyBinding:
  val IdentifierOrdering = Ordering.by[String,Tuple2[Int,String]](s => (s.length, s))

/**
 * Implementations must ensure that identifiers are always sorted by AnyBinding.IdentifierOrdering
 */
trait AnyBinding:
  def siteRootedPath : Rooted
  def identifiers    : immutable.SortedSet[String]
