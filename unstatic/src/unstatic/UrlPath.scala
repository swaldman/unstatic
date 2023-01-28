package unstatic

import java.net.URL
import scala.annotation.tailrec

object UrlPath:
  object Abs:
    def parse( url : String ) : Abs = apply(url)
    def apply( url : String ) : Abs = apply(URL(url))
    def apply( url : URL    ) : Abs = Abs(URL(url,"/"), Rooted.parse(url.getPath) )
  final case class Abs private[UrlPath] ( server : URL, path : Rooted ) extends UrlPath:
    def resolve(relpath: UrlPath.Rel): Abs = this.copy( path = path.resolve(relpath) )
    def resolveSibling(relpath: UrlPath.Rel): Abs = this.copy( path = path.resolveSibling(relpath) )
    def relativize( other : Abs ) : Rel =
      if (this.server == other.server) then
        path.relativize(other.path)
      else
        throw new CannotRelativize(s"'${this}' and '${other}' do not share the same server.")
    def embedRoot(rooted : UrlPath.Rooted): Abs = resolve(rooted.unroot)
    override def toString() : String = server.toString() + path.toString().substring(1)

  trait PathPart[T <: PathPart[T]] extends UrlPath:
    self : T =>
    def elements: Vector[String]
    private[UrlPath] def withElements( elements : Vector[String] ) : T
    def resolve(relpath: Rel): T = this.withElements( this.elements ++ relpath.elements )
    def resolveSibling(relpath: Rel): T = this.withElements( this.elements.init ++ relpath.elements ) // will throw if we're empty!
    def relativize( other : T ) : UrlPath.Rel =
      val shared = this.elements.zip(other.elements).takeWhile(tup => tup(0) == tup(1)).map(_(0))
      Rel( Array.fill(elements.length - shared.length)("..").to(Vector) ++ other.elements.drop(shared.length) )
    def relativizeSibling( other : T ) : UrlPath.Rel = this.parent.relativize(other)
    def embedRoot(rooted : UrlPath.Rooted): T = resolve(rooted.unroot)
    def canonical : T =
      val elements = _dedottifySuffix( this.elements )
      assert(
        !this.isRooted || !dotDotHead(elements),
        s"Rooted path '${this}' would escape its root. Should not have been constructable."
      )
      this.withElements( elements )
    def parentOption : Option[T] =
      (this.isDotty, this.isRooted) match {
        case (true, true) =>
          val check = this.elements :+ ".."
          if Rooted.wouldEscapeRoot(check) then None else Some(this.withElements(check))
        case(true, false) =>
          Some(this.withElements(this.elements :+ ".."))
        case (false, true) =>
          if this.elements.nonEmpty then Some(this.withElements(this.elements.init)) else None
        case (false, false) =>
          if this.elements.nonEmpty then Some(this.withElements(this.elements.init)) else Some(this.withElements(Vector("..")))
      }
    def parent : T = parentOption.getOrElse {
      throw new BadPathException("Tried to take parent of root on a rooted path.")
    }
    def isDotty : Boolean = elements.exists( e => e == "." || e == ".." )
    def isRooted : Boolean
    override def toString() : String = elements.mkString("/")
  end PathPart

  object Rooted:
    private def preparse( path : String, strict : Boolean ) : Vector[String] =
      if strict && (path.isEmpty || path(0) != '/') then
        throw new BadPathException(s"Putative rooted path '${path}' must begin with '/'.")
      path.split("""\/+""").filter(_.nonEmpty).to(Vector)

    private[UrlPath] def wouldEscapeRoot( elements : Vector[String]) : Boolean = dotDotHead(_dedottifySuffix(elements))

    private def guard( elements : Vector[String] ) : Unit =
      val check = _dedottifySuffix(elements)
      if dotDotHead(check) then
        throw new BadPathException (
          s"""Resolving elements provided would escape root of Rooted path. Given path: '${elements.mkString("/","/","")}', Resolved path: '${Rel.fromElements(check : _*)}'"""
        )

    val root = Rooted(Vector.empty)

    def fromElements( elements : String* ) : Rooted = Rooted( elements.filter( _.nonEmpty ).to(Vector) )
    def parseAndRoot( path : String)       : Rooted = Rooted( preparse(path, false) )
    def parse( path : String )             : Rooted = Rooted( preparse(path, true) )
    def apply( path : String )             : Rooted = Rooted.parse(path)
    def apply( elements : Vector[String] ) : Rooted =
      guard(elements)
      new Rooted( elements )
  case class Rooted private[UrlPath] ( val elements : Vector[String] ) extends PathPart[Rooted]:
    private[UrlPath] def withElements( elements : Vector[String] ) : Rooted = this.copy(elements = elements)
    private[UrlPath] def aboveParent : Rooted = throw new BadPathException("Attempted to take the parent of a root path.")
    def unroot : Rel = Rel( this.elements )
    def isPrefixOf(other : Rooted) =
      other.elements.length >= this.elements.length && (0 until this.elements.length).forall( i => this.elements(i) == other.elements(i))
    def isRoot : Boolean = elements.isEmpty
    def isRooted : Boolean = true
    override def toString() : String = "/" + super.toString()

  object Rel:
    private def preparse(path: String): Vector[String] =
      if path.nonEmpty && path(0) == '/' then
        throw new BadPathException(s"Putative relative (unrooted) path '${path}' must not begin with '/'.")
      path.split("""\/+""").filter(_.nonEmpty).to(Vector)
    def parse( path : String ) : Rel = Rel(preparse(path))
    def apply( path : String ) : Rel = parse(path)
    def fromElements( elements : String* ) : Rel = Rel( elements.filter( _.nonEmpty ).to(Vector) )
    val here = Rel(Vector.empty)
  case class Rel private[UrlPath](elements: Vector[String]) extends PathPart[Rel]:
    private[UrlPath] def withElements( elements : Vector[String] ) : Rel = this.copy(elements = elements)
    def isRooted : Boolean = false

  private val SomeDotDot = Some("..")
  def dotDotHead( elements : Vector[String] ) : Boolean =
    elements.headOption == SomeDotDot

  private[UrlPath] def _dedottifySuffix( elements : Vector[String] ): Vector[String] =
    val dedot1 = elements.filter(_ != ".")
    if (dedot1.nonEmpty) then
      val (reversed, extras) = deDoubleDottifyReversed(dedot1.toList.reverse, 0, Nil)
      if extras == 0 then
        reversed.reverse.toVector
      else
        (List.fill(extras)("..") ::: reversed.reverse).toVector
    else
      dedot1

  // single dots already removed!
  @tailrec
  private def deDoubleDottifyReversed( inReversed : List[String], unspentDoubleDots : Int, outReversed : List[String] ) : (List[String], Int) =
    inReversed match
      case ".." :: tail                           => deDoubleDottifyReversed(tail, unspentDoubleDots + 1, outReversed)
      case head :: tail if unspentDoubleDots >  0 => deDoubleDottifyReversed(tail, unspentDoubleDots - 1, outReversed)
      case head :: tail if unspentDoubleDots == 0 => deDoubleDottifyReversed(tail, unspentDoubleDots, head :: outReversed)
      case head :: tail                           => throw new AssertionError(s"Should be unreachable. negative unspentDoubleDots? ${unspentDoubleDots}")
      case Nil => (outReversed, unspentDoubleDots)

  // TODO: Better validation that "absolute" paths are
  //       valid absolute URLs
  private def isAbsolute( path : String ) : Boolean =
    val colonIndex = path.indexOf(':')
    val slashIndex = path.indexOf('/')
    ( colonIndex, slashIndex ) match
      case (-1, -1) => false
      case ( _, -1) => true
      case (-1, _ ) => false
      case (ci, si) => ci < si

  def parse( path : String ) : UrlPath =
    if (isAbsolute(path)) then
      Abs(path)
    else if path.isEmpty || path(0) != '/' then
      Rel(path)
    else Rooted(path)

sealed trait UrlPath:
  def resolve(relpath: UrlPath.Rel): UrlPath
  def resolveSibling(relpath: UrlPath.Rel): UrlPath
  def embedRoot(rooted : UrlPath.Rooted): UrlPath


