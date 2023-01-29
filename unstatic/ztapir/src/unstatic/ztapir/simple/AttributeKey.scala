package unstatic.ztapir.simple

import scala.collection.*

object Attribute:
  object Key:
    object Converter:
      val SimpleString : Converter[String] =
        (a : Any) =>
          a match
            case s : String => Some(s.trim)
            case _          => None
      val StringList : Converter[List[String]]  =
        (a : Any) =>
          a match
            case s : String => Some(s.split(",").map(_.trim).toList)
            case _          => None
    type Converter[T] = Any => Option[T]
  enum Key[T](val converter : Key.Converter[T], val variations : List[String]):
    case `Title`  extends Key[String]      (Key.Converter.SimpleString, Nil)
    case `Author` extends Key[List[String]](Key.Converter.StringList, "Authors" :: Nil)
    case `Tag`    extends Key[List[String]](Key.Converter.StringList, "Tags" :: Nil)

  object Checkable:
    def from( map : immutable.Map[String,Any] ) : Checkable =
      new Checkable( map.map { case (k,v) => (lowerCased(k),v) } )
  case class Checkable private ( lcMap : immutable.Map[LowerCased,Any] ):
    def check[T](key : Key[T]) : Option[T] =
      val lcs = (key.toString :: key.variations).map(lowerCased)
      val convert : Key.Converter[T] = key.converter
      def find( list : List[LowerCased] ) : Option[T] =
        list match
          case head :: tail => lcMap.get(head).flatMap(convert) orElse find(tail)
          case Nil          => None
      find(lcs)

