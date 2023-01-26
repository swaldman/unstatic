package unstatic

import scala.collection.*
import unstatic.UrlPath.*

trait Site extends StaticLocationBinding.Source:
  def serverUrl : Abs
  def basePath  : Rooted
  def sitePath  : Abs = serverUrl.embedRoot(basePath)

  def siteRoot = sitePath

  def serverRootedPath( fromSiteRootedPath : Rooted ) : Rooted = basePath.embedRoot( fromSiteRootedPath )
  def serverRootedPath( fromSiteRootedPath : String ) : Rooted = serverRootedPath( Rooted(fromSiteRootedPath) )

trait StaticResources[S <: Site] extends StaticLocationBinding.Source:
  val site : S
  def locationBindings : immutable.Seq[StaticLocationBinding]

private val ToDashChar = immutable.Set(' ','-')
private val isWordChar = Character.isJavaIdentifierPart

def linkableTitle( title : String ) =
  title.toLowerCase.filter( c => isWordChar(c) || ToDashChar(c) ).map( (c : Char) => if ToDashChar(c) then '-' else c )







