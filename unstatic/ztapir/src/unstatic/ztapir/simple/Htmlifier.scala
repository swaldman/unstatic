package unstatic.ztapir.simple

object Htmlifier:
  val identity : Htmlifier = (s : String, opts : Options) => s
  case class Options( generatorFullyQualifiedName : Option[String] )
type Htmlifier = Function2[String,Htmlifier.Options,String]

