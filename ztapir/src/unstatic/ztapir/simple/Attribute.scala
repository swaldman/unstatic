package unstatic.ztapir.simple

import untemplate.*

import java.time.Instant

import scala.util.Try
import scala.collection.{immutable, IterableOnce}

import audiofluidity.rss.Element

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
      private def unexpectedType(key : String, expected : String, found : Any) : Nothing =
        throw new BadAttributeException( s"${key}: Unexpected type. Expected ${expected}. Found: ${found}" )
      val HintAnnouncePolicy : Converter[Element.Iffy.HintAnnounce.Policy] =
        (key : String, a : Any) =>
          a match
            case policy : Element.Iffy.HintAnnounce.Policy => policy
            case s : String => Element.Iffy.HintAnnounce.Policy.lenientParse(s).getOrElse( throw new BadAttributeException( s"'$s' is not a valid iffy:hint-announce policy." ) )
            case other      => unexpectedType(key, "String or Element.Iffy.HintAnnounce.Policy", other)
      val SimpleString : Converter[String] =
        (key : String, a : Any) =>
          a match
            case s : String => s.trim
            case other      => unexpectedType(key, "String", other)
      val StringList : Converter[List[String]] =
        (key : String, a : Any) =>
          a match
            case list : List[String] @ unchecked =>
              if list.forall( _.isInstanceOf[String] ) then // gotta check at runtime
                list
              else
                throw new BadAttributeException( s"${key}: At least one element is not a String: ${list}" )
            case s : String       => s.split(",").map(_.trim).toList
            case other            => unexpectedType(key, "List[String] or comma-delimited String", other)
      val Timestamp : Converter[Instant] =
        (key : String, a : Any) =>
          a match
            case s : String  => parseTimestamp(s).recover( t => throw new BadAttributeException( s"${key}: '$s' could not be parsed as a timestamp.", t) ).get
            case i : Instant => i
            case other       => unexpectedType(key, "Instant or timestamp as String", other)
      val SimpleBoolean : Converter[Boolean] =
        (key : String, a : Any) =>
          a match
            case b : Boolean => b
            case s : String  => Try( s.toBoolean ).recover( t => throw new BadAttributeException( s"${key}: '$s' could not be coerced to a Boolean value as required.", t) ).get
            case other       => unexpectedType(key, "Boolean or boolean value as String", other)
      val UpdateRecords : Converter[immutable.SortedSet[UpdateRecord]] =
        (key : String, a : Any) =>
          a match
            case io : IterableOnce[UpdateRecord] @unchecked => // have to check at runtime
              val list = io.iterator.to(List)
              if list.forall( _.isInstanceOf[UpdateRecord] ) then
                immutable.SortedSet.from(list)
              else
                throw new BadAttributeException( s"${key}: At least one element is not an UpdateRecord: ${io}" )
            case other => unexpectedType(key, "an iterable collection of UpdateRecord", other)
    type Converter[T] = (String, Any) => T
    abstract class Abstract[T](val converter : Key.Converter[T], val variations : List[String]):
      private lazy val allNames = (this.toString :: this.variations)
      private lazy val lcs = allNames.map(LowerCased.apply)
      private def check[X]( map : immutable.Map[X,Any], allKeys : List[X] ) : Option[T] =
        def find( list : List[X] ) : Option[T] =
          list match
            case head :: tail => map.get(head).map(value => converter(head.toString,value)) orElse find(tail)
            case Nil          => None
        find(allKeys)
      def caseInsensitiveCheck(ut : Untemplate.AnyUntemplate) : Option[T] = check(ut.UntemplateAttributesLowerCased, lcs)
      def caseSensitiveCheck(ut : Untemplate.AnyUntemplate) : Option[T] = check(ut.UntemplateAttributes, allNames)
  enum Key[T]( converter : Key.Converter[T], variations : List[String] ) extends Key.Abstract[T](converter, variations):
    case `Title`              extends Key[String]                           (Key.Converter.SimpleString,                       Nil)
    case `Author`             extends Key[List[String]]                     (Key.Converter.StringList,    "Authors"         :: Nil)
    case `InitialAuthor`      extends Key[List[String]]                     (Key.Converter.StringList,    "InitialAuthors"  :: Nil)
    case `Tag`                extends Key[List[String]]                     (Key.Converter.StringList,    "Tags"            :: Nil)
    case `PubDate`            extends Key[Instant]                          (Key.Converter.Timestamp,     "PublicationDate" :: Nil)
    case `Content-Type`       extends Key[String]                           (Key.Converter.SimpleString,  "ContentType"     :: Nil)
    case `Permalink`          extends Key[String]                           (Key.Converter.SimpleString,                       Nil)
    case `MediaDir`           extends Key[String]                           (Key.Converter.SimpleString,                       Nil)
    case `LinkName`           extends Key[String]                           (Key.Converter.SimpleString,                       Nil)
    case `Anchor`             extends Key[String]                           (Key.Converter.SimpleString,  "Uid"             :: Nil)
    case `UpdateHistory`      extends Key[immutable.SortedSet[UpdateRecord]](Key.Converter.UpdateRecords,                      Nil)
    case `Sprout`             extends Key[Boolean]                          (Key.Converter.SimpleBoolean,                      Nil) // see https://v5.chriskrycho.com/essays/feeds-are-not-fit-for-gardening/
    case `HintAnnouncePolicy` extends Key[Element.Iffy.HintAnnounce.Policy] (Key.Converter.HintAnnouncePolicy,                 Nil)
    case `SyntheticType`      extends Key[String]                           (Key.Converter.SimpleString,                       Nil) // intended only for synthetic untemplates!

