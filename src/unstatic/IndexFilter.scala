package unstatic

import scala.collection.*
import untemplate.Untemplate.AnyUntemplate

object IndexFilter:
  def fromIndex( untemplateIndex : immutable.Map[String,AnyUntemplate] ) : IndexFilter =
    IndexFilter(untemplateIndex.map { case(k,v) => (immutable.ArraySeq.unsafeWrapArray(k.split('.')), v)}.toMap)
case class IndexFilter( workingSet : immutable.Map[immutable.ArraySeq[String],AnyUntemplate] ):
  def inOrBeneathPackage(dottyPackage : String) : IndexFilter =
    IndexFilter( workingSet.filter { case(k,_) => k.startsWith(immutable.ArraySeq.unsafeWrapArray(dottyPackage.split('.')), 0) } )
  def directlyInPackage(dottyPackage : String)  : IndexFilter =
    val pkg = immutable.ArraySeq.unsafeWrapArray(dottyPackage.split('.'))
    IndexFilter( workingSet.filter { case (k,_) => k.length == pkg.length+1 && k.startsWith(pkg,0) } )
  def withNameLike( predicate : String => Boolean) : IndexFilter =
    IndexFilter( workingSet.filter { case(k,_) => predicate(k.last) } )
  def untemplates : immutable.Set[AnyUntemplate] = workingSet.values.toSet
  def filteredIndex : immutable.SortedMap[String,AnyUntemplate] = workingSet.map { case (k,v) => (k.mkString("."), v) }.to(immutable.SortedMap)



