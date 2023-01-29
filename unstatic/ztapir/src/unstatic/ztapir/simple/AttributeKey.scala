package unstatic.ztapir.simple

import java.time.Instant
import scala.collection.*

// XXX: Here is some of where I'd like to warn,
//      if things aren't found or aren't of
//      expected type. For now we fail silently,
//      which seems not great.

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
      val Timestamp : Converter[Instant]  =
        (a : Any) =>
          a match
            case s : String  => parseTimestamp(s).toOption
            case i : Instant => Some(i)
            case _           => None
    type Converter[T] = Any => Option[T]
  enum Key[T](val converter : Key.Converter[T], val variations : List[String]):
    case `Title`        extends Key[String]      (Key.Converter.SimpleString,                      Nil)
    case `Author`       extends Key[List[String]](Key.Converter.StringList,   "Authors"         :: Nil)
    case `Tag`          extends Key[List[String]](Key.Converter.StringList,   "Tags"            :: Nil)
    case `PubDate`      extends Key[Instant]     (Key.Converter.Timestamp,    "PublicationDate" :: Nil)
    case `Content-Type` extends Key[String]      (Key.Converter.SimpleString, "ContentType"     :: Nil)
    case `Permalink`    extends Key[String]      (Key.Converter.SimpleString,                      Nil)
    case `LinkName`     extends Key[String]      (Key.Converter.SimpleString,                      Nil)
  object Checkable:
    def from( map : immutable.Map[String,Any] ) : Checkable =
      new Checkable( map.map { case (k,v) => (lowerCased(k),v) } )
    def from( ut : untemplate.Untemplate[?,?] ) : Checkable =
      from( ut.UntemplateAttributes )
  case class Checkable private ( private val lcMap : immutable.Map[LowerCased,Any] ):
    def check[T](key : Key[T]) : Option[T] =
      val lcs = (key.toString :: key.variations).map(lowerCased)
      val convert : Key.Converter[T] = key.converter
      def find( list : List[LowerCased] ) : Option[T] =
        list match
          case head :: tail => lcMap.get(head).flatMap(convert) orElse find(tail)
          case Nil          => None
      find(lcs)

