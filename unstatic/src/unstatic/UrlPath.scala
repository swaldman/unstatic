package unstatic

import java.net.{URL, URLEncoder}

object UrlPath:
  object Abs:
    def parse( url : String ) : Abs = apply(url)
    def apply( url : String ) : Abs = apply(URL(url))
    def apply( url : URL    ) : Abs = Abs(URL(url,"/"), Rooted.parse(url.getPath) )
  final case class Abs private[unstatic] ( server : URL, path : Rooted ) extends UrlPath:
    def resolve(relpath: UrlPath.Rel): Abs = this.copy( path = path.resolve(relpath) )
    def resolveSibling(relpath: UrlPath.Rel): Abs = this.copy( path = path.resolveSibling(relpath) )
    def relativize( other : Abs ) : Rel =
      if (this.server == other.server) then
        path.relativize(other.path)
      else
        throw new CannotRelativize(s"'${this}' and '${other}' do not share the same server.")
    def reroot(rooted : UrlPath.Rooted): Abs = resolve(rooted.unroot)
    override def toString() : String = server.toString() + path.toString().substring(1)

  trait PathPart[T <: PathPart[T]] extends UrlPath:
    def elements: Vector[String]
    private[unstatic] def withElements( elements : Vector[String] ) : T
    def resolve(relpath: Rel): T = this.withElements( this.elements ++ relpath.elements )
    def resolveSibling(relpath: Rel): T = this.withElements( this.elements.init ++ relpath.elements ) // will throw if we're empty!
    def relativize( other : T ) : UrlPath.Rel =
      val shared = this.elements.zip(other.elements).takeWhile(tup => tup(0) == tup(1)).map(_(0))
      Rel( Array.fill(elements.length - shared.length)("..").to(Vector) ++ other.elements )
    def reroot(rooted : UrlPath.Rooted): T = resolve(rooted.unroot)
    def dedottify : T =
      val dedot1 = elements.filter( _ == ".")
      if dedot1(0) == ".." then
        throw new CannotDedottify(s"Can't dedottify '${this}', can't resolve initial path element.")
      val offset = "DUMMY" +: dedot1
      val offsetSnipped = dedot1.zip(offset).filter( tup => tup(0) != ".." ).map( tup => tup(1) ).filter( _ != ".." )
      this.withElements( offsetSnipped.tail )

    override def toString() : String = elements.map( URLEncoder.encode(_, scala.io.Codec.UTF8.charSet) ).mkString("/")
  end PathPart

  object Rooted:
    private def preparse( path : String ) : Vector[String] =
      if path.isEmpty || path(0) != '/' then
        throw new BadPathException(s"Putative rooted path '${path}' must begin with '/'.")
      path.split("""\/+""").filter(_.nonEmpty).to(Vector)

    def fromElements( elements : String* ) : Rooted = Rooted( elements.filter( _.nonEmpty ).to(Vector) )
    def parse( path : String )             : Rooted = Rooted( preparse(path) )
    def apply( path : String )             : Rooted = Rooted.parse(path)
  case class Rooted private[unstatic] ( val elements : Vector[String] ) extends PathPart[Rooted]:
    private[unstatic] def withElements( elements : Vector[String] ) : Rooted = this.copy(elements = elements)
    def unroot : Rel = Rel( this.elements )
    override def toString() : String = "/" + super.toString()

  object Rel:
    private def preparse(path: String): Vector[String] =
      if path.nonEmpty && path(0) == '/' then
        throw new BadPathException(s"Putative relative (unrooted) path '${path}' must not begin with '/'.")
      path.split("""\/+""").filter(_.nonEmpty).to(Vector)
    def parse( path : String ) : Rel = Rel(preparse(path))
    def apply( path : String ) : Rel = parse(path)
    def fromElements( elements : String* ) : Rel = Rel( elements.filter( _.nonEmpty ).to(Vector) )
  case class Rel private[unstatic](elements: Vector[String]) extends PathPart[Rel]:
    private[unstatic] def withElements( elements : Vector[String] ) : Rel = this.copy(elements = elements)

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
  def reroot(rooted : UrlPath.Rooted): UrlPath


