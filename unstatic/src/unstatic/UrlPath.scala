package unstatic

import java.net.{URL, URLEncoder}

object UrlPath:
  object Abs:
    def parse( url : String ) : Abs = apply(url)
    def apply( url : String ) : Abs = apply(URL(url))
    def apply( url : URL    ) : Abs = Abs(URL(url,"/"), Rooted.parse(url.getPath) )
  final case class Abs private[unstatic] ( server : URL, path : Rooted ) extends UrlPath:
    def resolve(unrooted: UrlPath.Unrooted): UrlPath.Abs = this.copy( path = path.resolve(unrooted) )
    def resolveSibling(unrooted: UrlPath.Unrooted): UrlPath.Abs = this.copy( path = path.resolveSibling(unrooted) )
    def relativize( other : Abs ) : Unrooted =
      if (this.server == other.server) then
        path.relativize(other.path)
      else
        throw new CannotRelativize(s"'${this}' and '${other}' do not share the same server.")
    override def toString() : String = server.toString() + path.toString().substring(1)

  trait PathPart[T <: PathPart[T]] extends UrlPath:
    def elements: Vector[String]
    private[unstatic] def withElements( elements : Vector[String] ) : T
    def resolve(unrooted: Unrooted): T = this.withElements( this.elements ++ unrooted.elements )
    def resolveSibling(unrooted: Unrooted): T = this.withElements( this.elements.init ++ unrooted.elements ) // will throw if we're empty!
    def relativize( other : T ) : UrlPath.Unrooted =
      val shared = this.elements.zip(other.elements).takeWhile(tup => tup(0) == tup(1)).map(_(0))
      Unrooted( Array.fill(elements.length - shared.length)("..").to(Vector) ++ other.elements )
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
    def unroot : Unrooted = Unrooted( this.elements )
    override def toString() : String = "/" + super.toString()

  object Unrooted:
    private def preparse(path: String): Vector[String] =
      if path.nonEmpty && path(0) == '/' then
        throw new BadPathException(s"Putative unrooted path '${path}' must not begin with '/'.")
      path.split("""\/+""").filter(_.nonEmpty).to(Vector)
    def parse( path : String ) : Unrooted = Unrooted(preparse(path))
    def apply( path : String ) : Unrooted = parse(path)
    def fromElements( elements : String* ) : Unrooted = Unrooted( elements.filter( _.nonEmpty ).to(Vector) )
  case class Unrooted private[unstatic](elements: Vector[String]) extends PathPart[Unrooted]:
    private[unstatic] def withElements( elements : Vector[String] ) : Unrooted = this.copy(elements = elements)

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
      Unrooted(path)
    else Rooted(path)

sealed trait UrlPath:
  def resolve(unrooted: UrlPath.Unrooted): UrlPath
  def resolveSibling(unrooted: UrlPath.Unrooted): UrlPath


