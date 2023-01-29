package unstatic

import scala.collection.*
import unstatic.UrlPath.*

trait StaticResources[S <: Site] extends StaticLocationBinding.Source:
  val site : S
  def locationBindings : immutable.Seq[StaticLocationBinding]

