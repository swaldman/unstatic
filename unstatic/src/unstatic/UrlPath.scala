package unstatic

import java.net.URL
import scala.annotation.tailrec

object UrlPath:
  object Abs:
    def parse( url : String ) : Abs = apply(url)
    def apply( url : String ) : Abs = apply(URL(url))
    def apply( url : URL    ) : Abs = Abs(URL(url,"/"), Rooted.parse(url.getPath) )
  final case class Abs private[UrlPath] ( server : URL, path : Rooted ) extends UrlPath:
    def representsDir = path.representsDir
    def serverRoot : Abs = this.copy(path = Rooted.root)
    def resolve(relpath: UrlPath.Rel): Abs = this.copy( path = path.resolve(relpath) )
    def resolveSibling(relpath: UrlPath.Rel): Abs = this.copy( path = path.resolveSibling(relpath) )
    def relativize( other : Abs ) : Rel =
      if (this.server == other.server) then
        path.relativize(other.path)
      else
        throw new CannotRelativize(s"'${this}' and '${other}' do not share the same server.")
    def embedRoot(rooted : UrlPath.Rooted): Abs = resolve(rooted.unroot)
    def parentOption : Option[Abs] = path.parentOption.map(p => this.copy(path=p))
    def parent : Abs = parentOption.getOrElse {
      throw new BadPath("Tried to take parent of server root on an absolute UrlPath.")
    }
    override def toString() : String = server.toString() + path.toString().substring(1)

  trait PathPart[T <: PathPart[T]] extends UrlPath:
    self : T =>
    def elements: Vector[String]
    private[UrlPath] def withElements( elements : Vector[String] ) : T
    private[UrlPath] def withRepresentsDir( representsDir : Boolean ) : T
    private[UrlPath] def withElementsRepresentsDir( elements : Vector[String], representsDir : Boolean ) : T

    // Note: we validate in withRepresentsDir(...) to prevent invalid not-dir paths
    def asDir : T = if this.representsDir then this else this.withRepresentsDir(true)
    def asNotDir : T = if this.representsDir then this.withRepresentsDir(false) else this

    def resolve(relpath: Rel): T = this.withElementsRepresentsDir( this.elements ++ relpath.elements, relpath.representsDir )
    def resolveSibling(relpath: Rel): T = this.withElementsRepresentsDir( this.elements.init ++ relpath.elements, relpath.representsDir) // will throw if we're empty!
    def resolve(relpath : String) : T = this.resolve(Rel(relpath))
    def resolveSibling(relpath : String) : T = this.resolveSibling(Rel(relpath))
    def relativize( other : T ) : UrlPath.Rel =
      val shared = this.elements.zip(other.elements).takeWhile(tup => tup(0) == tup(1)).map(_(0))
      Rel( Array.fill(elements.length - shared.length)("..").to(Vector) ++ other.elements.drop(shared.length), other.representsDir )
    def relativizeSibling( other : T ) : UrlPath.Rel = this.parent.relativize(other)
    def embedRoot(rooted : UrlPath.Rooted) : T = resolve(rooted.unroot)
    def canonical : T =
      val elements = _dedottifySuffix( this.elements )
      assert(
        !this.isRooted || !dotDotHead(elements),
        s"Rooted path '${this}' would escape its root. Should not have been constructable."
      )
      this.withElements( elements )
    def _parentOption : Option[T] =
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
    def parentOption : Option[T] = _parentOption.map( _.asDir )
    def parent : T = parentOption.getOrElse {
      throw new BadPath("Tried to take parent of root on a rooted path.")
    }
    def isDotty : Boolean = elements.exists( e => e == "." || e == ".." )
    def isRooted : Boolean
    override def toString() : String = if elements.nonEmpty && representsDir then elements.mkString("","/","/") else elements.mkString("/")
  end PathPart

  object Rooted:
    private def preparse( path : String, strict : Boolean ) : ( Vector[String], Boolean ) =
      if strict && (path.isEmpty || path(0) != '/') then
        throw new BadPath(s"Putative rooted path '${path}' must begin with '/'.")
      val realElements = path.split("""\/+""").filter(_.nonEmpty).to(Vector)
      val representsDir = path.endsWith("/") || path.isEmpty || dottyLast(realElements)
      ( realElements, representsDir )

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
      val ( elements, representsDir ) = preparse(path, false)
      Rooted( elements, representsDir )
    def parse( path : String ) : Rooted =
      val ( elements, representsDir ) = preparse(path, true)
      Rooted( elements, representsDir )
    def apply( path : String ) : Rooted = Rooted.parse(path)
    def apply( elements : Vector[String] ) : Rooted =
      val realElements = elements.filter( _.nonEmpty ).to(Vector)
      if realElements.isEmpty then
        root
      else
        if dottyLast(elements) then new Rooted(realElements, true) else new Rooted( realElements, false )
    // guard all constructor and copy calls (except those of root!) with this to prevent the
    // possibility of invalid Rooteds
    private[UrlPath] def validateCreateOrThrowMaybeSubstitute( elements : Vector[String], representsDir : Boolean ) : Option[Rooted] =
      if elements.isEmpty then
        if representsDir then Some(root) else throw new MustRepresentDirectory("Cannot create root (empty path) element that does not represent a directory")
      else
        val last = elements.last
        if !representsDir && (last == "." || last == "..") then
          throw new MustRepresentDirectory("A path ending in '.' or '..' must represent a directory.")
        else
          guard(elements)
        None
    def apply( elements : Vector[String], representsDir : Boolean ) : Rooted =
      validateCreateOrThrowMaybeSubstitute(elements, representsDir).getOrElse( new Rooted( elements, representsDir ) )
  case class Rooted private[UrlPath] ( elements : Vector[String], representsDir : Boolean ) extends PathPart[Rooted]:
    private[UrlPath] def withElements( elements : Vector[String] ) : Rooted =
      Rooted.validateCreateOrThrowMaybeSubstitute(elements, this.representsDir).getOrElse(this.copy(elements = elements))
    private[UrlPath] def withRepresentsDir( representsDir : Boolean ) : Rooted =
      Rooted.validateCreateOrThrowMaybeSubstitute( this.elements, representsDir ).getOrElse( this.copy(representsDir = representsDir) )
    private[UrlPath] def withElementsRepresentsDir( elements : Vector[String], representsDir : Boolean ) : Rooted =
      Rooted.apply( elements, representsDir)
    private[UrlPath] def aboveParent : Rooted = throw new BadPath("Attempted to take the parent of a root path.")
    def unroot : Rel = Rel( this.elements )
    def isPrefixOf(other : Rooted) =
      other.elements.length >= this.elements.length && (0 until this.elements.length).forall( i => this.elements(i) == other.elements(i))
    def isRoot : Boolean = elements.isEmpty
    def isRooted : Boolean = true
    override def toString() : String = "/" + super.toString()

  object Rel:
    private def preparse(path: String): (Vector[String], Boolean ) =
      if path.nonEmpty && path(0) == '/' then
        throw new BadPath(s"Putative relative (unrooted) path '${path}' must not begin with '/'.")
      else
        val realElements = path.split("""\/+""").filter(_.nonEmpty).to(Vector)
        val representsDir = path.endsWith("/") || path.isEmpty || dottyLast(realElements)
        ( realElements, representsDir )
    def parse( path : String ) : Rel =
      val (elements, representsDir ) = preparse(path)
      apply( elements, representsDir )
    def apply( path : String ) : Rel = parse(path)
    def apply( elements : Vector[String] ) : Rel =
      if elements.isEmpty then here else apply( elements, false )
    private[UrlPath] def validateCreateOrThrowMaybeSubstitute( elements : Vector[String], representsDir : Boolean ) : Option[Rel] =
      if elements.isEmpty then
        if representsDir then Some(here) else throw new MustRepresentDirectory("An empty UrlPath.Rel can only represent a directory!")
      else
        None
    def apply( elements : Vector[String], representsDir : Boolean ) : Rel =
      validateCreateOrThrowMaybeSubstitute( elements, representsDir ).getOrElse(new Rel( elements, representsDir ))
    def fromElements( elements : String* ) : Rel =
      val realElements = elements.filter( _.nonEmpty ).to(Vector)
      if realElements.isEmpty then here else Rel( realElements, false )
    val here = new Rel(Vector.empty, true) // use the raw constructor to avoid endless recursion in apply
  case class Rel private[UrlPath](elements: Vector[String], representsDir : Boolean) extends PathPart[Rel]:
    private[UrlPath] def withElements( elements : Vector[String] ) : Rel =
      Rel.validateCreateOrThrowMaybeSubstitute(elements, this.representsDir).getOrElse(this.copy(elements = elements))
    private[UrlPath] def withRepresentsDir( representsDir : Boolean ) : Rel =
      Rel.validateCreateOrThrowMaybeSubstitute( this.elements, representsDir).getOrElse( this.copy(representsDir = representsDir) )
    private[UrlPath] def withElementsRepresentsDir( elements : Vector[String], representsDir : Boolean ) : Rel =
      Rel.apply( elements, representsDir)
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
  def representsDir : Boolean
  def resolve(relpath: UrlPath.Rel): UrlPath
  def resolveSibling(relpath: UrlPath.Rel): UrlPath
  def embedRoot(rooted : UrlPath.Rooted): UrlPath

