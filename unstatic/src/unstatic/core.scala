package unstatic

import scala.collection.*
import scala.annotation.tailrec
import unstatic.UrlPath.*
import untemplate.{Untemplate,UntemplateScala}

private val ToDashChar = immutable.Set(' ','-')
private val isWordChar = Character.isJavaIdentifierPart

def linkableTitle( title : String ) =
  title.toLowerCase.filter( c => isWordChar(c) || ToDashChar(c) ).map( (c : Char) => if ToDashChar(c) then '-' else c )

val FilePathChars = immutable.Set('/', '\\', ':')
def ensureNoFilePathChars(s: String) =
  assert(!s.exists(FilePathChars.apply), s"File path characters not permitted: ${s}")

def fqnSuffixes( ut : Untemplate.AnyUntemplate ) : List[String] =
  val fqn = ut.UntemplateFullyQualifiedName
  val fqnList = fqn.split('.').toList

  @tailrec
  def suffixes(name : List[String], accum : List[List[String]]) : List[List[String]] =
    name match
      case head :: tail => suffixes(tail, accum :+ name)
      case Nil => accum

  suffixes(fqnList, Nil).map( _.mkString(".") )

trait AnyBinding:
  def siteRootedPath : Rooted
  def identifiers    : immutable.Set[String]







