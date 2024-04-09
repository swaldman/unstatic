package unstatic.ztapir.simple

import untemplate.*

import java.time.Instant
import scala.collection.*

// XXX: Here is some of where I'd like to warn,
//      if things aren't found or aren't of
//      expected type. For now we fail silently,
//      which seems not great.

object Attribute:
  object Key:
    extension ( ut : Untemplate.AnyUntemplate )
      def checkAttributeKeyStrict[T]( key : Attribute.Key[T] )  : Option[T] = key.caseSensitiveCheck( ut )
      def checkAttributeKey[T]( key : Attribute.Key[T] ) : Option[T] = key.caseInsensitiveCheck( ut )

    extension ( resolved : SimpleBlog#EntryResolved )
      def checkAttributeKeyStrict[T]( key : Attribute.Key[T] )  : Option[T] = key.caseSensitiveCheck( resolved.entryUntemplate )
      def checkAttributeKey[T]( key : Attribute.Key[T] ) : Option[T] = key.caseInsensitiveCheck( resolved.entryUntemplate )

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
            case s : String  => Some( parseTimestamp(s).get )
            case i : Instant => Some(i)
            case _           => None
    type Converter[T] = Any => Option[T]
    abstract class Abstract[T](val converter : Key.Converter[T], val variations : List[String]):
      lazy val allNames = (this.toString :: this.variations)
      lazy val lcs = allNames.map(LowerCased.apply)
      def caseInsensitiveCheck(ut : Untemplate.AnyUntemplate) : Option[T] =
        val lcMap = ut.UntemplateAttributesLowerCased
        def find( list : List[LowerCased] ) : Option[T] =
          list match
            case head :: tail => lcMap.get(head).flatMap(converter) orElse find(tail)
            case Nil          => None
        find(lcs)
      def caseSensitiveCheck(ut : Untemplate.AnyUntemplate) : Option[T] =
          val map = ut.UntemplateAttributes
          def find( list : List[String] ) : Option[T] =
            list match
              case head :: tail => map.get(head).flatMap(converter) orElse find(tail)
              case Nil          => None
          find(allNames)
  enum Key[T]( converter : Key.Converter[T], variations : List[String] ) extends Key.Abstract[T](converter, variations):
    case `Title`        extends Key[String]      (Key.Converter.SimpleString,                      Nil)
    case `Author`       extends Key[List[String]](Key.Converter.StringList,   "Authors"         :: Nil)
    case `Tag`          extends Key[List[String]](Key.Converter.StringList,   "Tags"            :: Nil)
    case `PubDate`      extends Key[Instant]     (Key.Converter.Timestamp,    "PublicationDate" :: Nil)
    case `Content-Type` extends Key[String]      (Key.Converter.SimpleString, "ContentType"     :: Nil)
    case `Permalink`    extends Key[String]      (Key.Converter.SimpleString,                      Nil)
    case `MediaDir`     extends Key[String]      (Key.Converter.SimpleString,                      Nil)
    case `LinkName`     extends Key[String]      (Key.Converter.SimpleString,                      Nil)
    case `Anchor`       extends Key[String]      (Key.Converter.SimpleString,             "Uid" :: Nil)

