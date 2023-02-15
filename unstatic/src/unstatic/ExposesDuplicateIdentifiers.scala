package unstatic

import scala.collection.*

trait ExposesDuplicateIdentifiers extends Site:
  def allBindings : immutable.Seq[AnyBinding]

  lazy val duplicateIdentifiers : immutable.Set[String] =
    allBindings.flatMap( _.identifiers ).groupBy(identity).filter(_(1).size > 1).map(_(0)).toSet