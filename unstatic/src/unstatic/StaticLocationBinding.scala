package unstatic

import UrlPath.*
import java.nio.file.Path as JPath
import scala.collection.*

object StaticLocationBinding:
  trait Source:
    def locationBindings : immutable.Seq[StaticLocationBinding]
case class StaticLocationBinding( siteRootedPath : Rooted, source : JPath, identifiers : immutable.Set[String] ) extends AnyBinding
