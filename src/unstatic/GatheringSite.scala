package unstatic

import scala.collection.*

object GatheringSite:
  trait Composite extends GatheringSite:
    def locationBindingSources : immutable.Seq[StaticLocationBinding.Source]
    override def locationBindings : immutable.Seq[StaticLocationBinding] = locationBindingSources.flatMap( _.locationBindings )
  end Composite
trait GatheringSite extends Site, StaticLocationBinding.Source
