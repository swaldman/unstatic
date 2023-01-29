package unstatic

import scala.collection.*
import unstatic.UrlPath.*

private val ToDashChar = immutable.Set(' ','-')
private val isWordChar = Character.isJavaIdentifierPart

def linkableTitle( title : String ) =
  title.toLowerCase.filter( c => isWordChar(c) || ToDashChar(c) ).map( (c : Char) => if ToDashChar(c) then '-' else c )

val FilePathChars = immutable.Set('/', '\\', ':')
def ensureNoFilePathChars(s: String) =
  assert(!s.exists(FilePathChars.apply), s"File path characters not permitted: ${s}")









