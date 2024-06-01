package unstatic

import java.net.URL
import scala.annotation.tailrec

object UrlPath:
  object Abs:
    def parse( url : String ) : Abs = apply(url)
    def apply( url : String ) : Abs = apply(URL(url))
    def apply( url : URL    ) : Abs = Abs(URL(url,"/"), Rooted.parse(url.getPath) )
  final case class Abs private[UrlPath] ( server : URL, path : Rooted ) extends UrlPath:
    def serverRoot : Abs = this.copy(path = Rooted.root)
    def resolve(relpath: String) : Abs = this.copy( path = path.resolve(relpath) )
    def resolve(relpath: UrlPath.Rel): Abs = this.copy( path = path.resolve(relpath) )
    def resolveSibling(relpath: String): Abs = this.copy( path = path.resolveSibling(relpath) )
    def resolveSibling(relpath: UrlPath.Rel): Abs = this.copy( path = path.resolveSibling(relpath) )
    def relativize( other : Abs ) : Rel =
      if (this.server == other.server) then
        path.relativize(other.path)
      else
        throw new CannotRelativize(s"'${this}' and '${other}' do not share the same server.")
    def relativizeSibling( other : Abs ) : UrlPath.Rel = this.parent.relativize(other)
    def embedRoot(rooted : UrlPath.Rooted): Abs = resolve(rooted.unroot)
    def parentOption : Option[Abs] = path.parentOption.map(p => this.copy(path=p))
    def parent : Abs = parentOption.getOrElse {
      throw new BadPath("Tried to take parent of server root on an absolute UrlPath.")
    }
    def isDir = path.isDir
    def isLeaf = !isDir
    def asDir : Abs = if path.isDir then this else this.copy( path = path.asDir )
    def asLeaf : Abs = if path.isDir then this.copy( path = path.asLeaf ) else this
    def withPath( newPath : Rooted ) : Abs = this.copy( path = newPath )
    override def toString() : String = server.toString() + path.toString().substring(1)

  trait PathPart[T <: PathPart[T]] extends UrlPath:
    self : T =>
    def elements: Vector[String]
    private[UrlPath] def withElements( elements : Vector[String] ) : T
    private[UrlPath] def withIsDir( isDir : Boolean ) : T
    private[UrlPath] def withElementsIsDir( elements : Vector[String], isDir : Boolean ) : T

    def isPrefixOf( other : T ) : Boolean =
      val mySize = this.elements.size
      other.elements.size > mySize && other.elements.take(mySize) == this.elements

    def isDir : Boolean
    def isLeaf : Boolean = !isDir

    // Note: we validate in withIsDir(...) to prevent invalid not-dir paths
    def asDir : T = if this.isDir then this else this.withIsDir(true)
    def asLeaf : T = if this.isDir then this.withIsDir(false) else this

    def resolve(relpath: Rel): T = this.withElementsIsDir( this.elements ++ relpath.elements, relpath.isDir )
    def resolveSibling(relpath: Rel): T = this.withElementsIsDir( this.elements.init ++ relpath.elements, relpath.isDir) // will throw if we're empty!
    def resolve(relpath : String) : T = this.resolve(Rel(relpath))
    def resolveSibling(relpath : String) : T = this.resolveSibling(Rel(relpath))
    def relativize( other : T ) : UrlPath.Rel =
      val shared = this.elements.zip(other.elements).takeWhile(tup => tup(0) == tup(1)).map(_(0))
      Rel( Array.fill(elements.length - shared.length)("..").to(Vector) ++ other.elements.drop(shared.length), other.isDir )
    def relativizeSibling( other : T ) : UrlPath.Rel = this.parent.relativize(other)
    def relativizeFromNearestDir( other : T ) : UrlPath.Rel = if (this.isDir) relativize(other) else relativizeSibling(other)
    def embedRoot(rooted : UrlPath.Rooted) : T = resolve(rooted.unroot)
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
          if Rooted.wouldEscapeRoot(check) then None else Some(this.withElementsIsDir(check, true))
        case(true, false) =>
          Some(this.withElementsIsDir(this.elements :+ "..", true))
        case (false, true) =>
          if this.elements.nonEmpty then Some(this.withElementsIsDir(this.elements.init, true)) else None
        case (false, false) =>
          if this.elements.nonEmpty then Some(this.withElementsIsDir(this.elements.init, true)) else Some(this.withElementsIsDir(Vector(".."), true))
      }
    def parent : T = parentOption.getOrElse {
      throw new BadPath("Tried to take parent of root on a rooted path.")
    }
    def isDotty : Boolean = elements.exists( e => e == "." || e == ".." )
    def isRooted : Boolean
    override def toString() : String = if elements.nonEmpty && isDir then elements.mkString("","/","/") else elements.mkString("/")
  end PathPart

  object Rooted:
    private def preparse( path : String, strict : Boolean ) : ( Vector[String], Boolean ) =
      if strict && (path.isEmpty || path(0) != '/') then
        throw new BadPath(s"Putative rooted path '${path}' must begin with '/'.")
      val realElements = path.split("""\/+""").filter(_.nonEmpty).to(Vector)
      val isDir = path.endsWith("/") || path.isEmpty || dottyLast(realElements)
      ( realElements, isDir )

    private[UrlPath] def wouldEscapeRoot( elements : Vector[String]) : Boolean = dotDotHead(_dedottifySuffix(elements))

    private def guard( elements : Vector[String] ) : Unit =
      val check = _dedottifySuffix(elements)
      if dotDotHead(check) then
        throw new BadPath (
          s"""Resolving elements provided would escape root of Rooted path. Given path: '${elements.mkString("/","/","")}', Resolved path: '${Rel.fromElements(check : _*)}'"""
        )

    val root = new Rooted(Vector.empty, true) // we'd better circumvent apply!

    def fromElements( elements : String* ) : Rooted = apply(elements.toVector)
    def parseAndRoot( path : String) : Rooted =
      val ( elements, isDir ) = preparse(path, false)
      Rooted( elements, isDir )
    def parse( path : String ) : Rooted =
      val ( elements, isDir ) = preparse(path, true)
      Rooted( elements, isDir )
    def apply( path : String ) : Rooted = Rooted.parse(path)
    def apply( elements : Vector[String] ) : Rooted =
      val realElements = elements.filter( _.nonEmpty ).to(Vector)
      if realElements.isEmpty then
        root
      else
        if dottyLast(elements) then new Rooted(realElements, true) else new Rooted( realElements, false )
    // guard all constructor and copy calls (except those of root!) with this to prevent the
    // possibility of invalid Rooteds
    private[UrlPath] def validateCreateOrThrowMaybeSubstitute( elements : Vector[String], isDir : Boolean ) : Option[Rooted] =
      if elements.isEmpty then
        if isDir then Some(root) else throw new MustRepresentDirectory("Cannot create root (empty path) element that does not represent a directory")
      else
        val last = elements.last
        if !isDir && (last == "." || last == "..") then
          throw new MustRepresentDirectory("A path ending in '.' or '..' must represent a directory.")
        else
          guard(elements)
        None
    def apply( elements : Vector[String], isDir : Boolean ) : Rooted =
      validateCreateOrThrowMaybeSubstitute(elements, isDir).getOrElse( new Rooted( elements, isDir ) )
  case class Rooted private[UrlPath] ( elements : Vector[String], isDir : Boolean ) extends PathPart[Rooted]:
    private[UrlPath] def withElements( elements : Vector[String] ) : Rooted =
      Rooted.validateCreateOrThrowMaybeSubstitute(elements, this.isDir).getOrElse(this.copy(elements = elements))
    private[UrlPath] def withIsDir( isDir : Boolean ) : Rooted =
      Rooted.validateCreateOrThrowMaybeSubstitute( this.elements, isDir ).getOrElse( this.copy(isDir = isDir) )
    private[UrlPath] def withElementsIsDir( elements : Vector[String], isDir : Boolean ) : Rooted =
      Rooted.apply( elements, isDir)
    private[UrlPath] def aboveParent : Rooted = throw new BadPath("Attempted to take the parent of a root path.")
    def unroot : Rel = Rel( this.elements )
    def isRoot : Boolean = elements.isEmpty
    def isRooted : Boolean = true
    override def toString() : String = "/" + super.toString()

  object Rel:
    private def preparse(path: String): (Vector[String], Boolean ) =
      if path.nonEmpty && path(0) == '/' then
        throw new BadPath(s"Putative relative (unrooted) path '${path}' must not begin with '/'.")
      else
        val realElements = path.split("""\/+""").filter(_.nonEmpty).to(Vector)
        val isDir = path.endsWith("/") || path.isEmpty || dottyLast(realElements)
        ( realElements, isDir )
    def parse( path : String ) : Rel =
      val (elements, isDir ) = preparse(path)
      apply( elements, isDir )
    def apply( path : String ) : Rel = parse(path)
    def apply( elements : Vector[String] ) : Rel =
      if elements.isEmpty then here else if dottyLast(elements) then apply(elements, true) else apply( elements, false )
    private[UrlPath] def validateCreateOrThrowMaybeSubstitute( elements : Vector[String], isDir : Boolean ) : Option[Rel] =
      if elements.isEmpty then
        if isDir then Some(here) else throw new MustRepresentDirectory("An empty UrlPath.Rel can only represent a directory!")
      else if !isDir && dottyLast(elements) then
        throw new MustRepresentDirectory("UrlPath.Rel that end in '.' or '..' must represent directories.")
      else
        None
    def apply( elements : Vector[String], isDir : Boolean ) : Rel =
      validateCreateOrThrowMaybeSubstitute( elements, isDir ).getOrElse(new Rel( elements, isDir ))
    def fromElements( elements : String* ) : Rel =
      val realElements = elements.filter( _.nonEmpty ).to(Vector)
      if realElements.isEmpty then here else Rel( realElements, false )
    val here = new Rel(Vector.empty, true) // use the raw constructor to avoid endless recursion in apply
  case class Rel private[UrlPath](elements: Vector[String], isDir : Boolean) extends PathPart[Rel]:
    private[UrlPath] def withElements( elements : Vector[String] ) : Rel =
      Rel.validateCreateOrThrowMaybeSubstitute(elements, this.isDir).getOrElse(this.copy(elements = elements))
    private[UrlPath] def withIsDir( isDir : Boolean ) : Rel =
      Rel.validateCreateOrThrowMaybeSubstitute( this.elements, isDir).getOrElse( this.copy(isDir = isDir) )
    private[UrlPath] def withElementsIsDir( elements : Vector[String], isDir : Boolean ) : Rel =
      Rel.apply( elements, isDir)
    def isRooted : Boolean = false

  private val SomeDotDot = Some("..")
  private def dotDotHead( elements : Vector[String] ) : Boolean =
    elements.headOption == SomeDotDot

  private def dottyLast( elements : Vector[String] ) : Boolean =
    val last = elements.last
    elements.nonEmpty && (last == "." || last == "..")

  /**
   *  Will fully dedottify paths that don't escape the starting level of the path.
   *
   *  Paths that do escape the starting level get ".." prepended.
   *
   *  There are no dots beyond the prefix of zero or more ".." elements.
   */
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
  def isDir : Boolean
  def resolve(relpath: UrlPath.Rel): UrlPath
  def resolveSibling(relpath: UrlPath.Rel): UrlPath
  def embedRoot(rooted : UrlPath.Rooted): UrlPath

