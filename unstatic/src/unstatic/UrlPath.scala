package unstatic

import java.net.{URL, URLEncoder}

object UrlPath:
  object Abs:
    def parse( url : String ) : Abs = apply(url)
    def apply( url : String ) : Abs = apply(URL(url))
    def apply( url : URL    ) : Abs = Abs(URL(url,"/"), Rooted.Server.parse(url.getPath) )
  final case class Abs private[unstatic] ( server : URL, path : Rooted.Server ) extends UrlPath:
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
    private[unstatic] def copy( elements : Vector[String] ) : T
    def resolve(unrooted: Unrooted): T = this.copy( this.elements ++ unrooted.elements )
    def resolveSibling(unrooted: Unrooted): T = this.copy( this.elements.init ++ unrooted.elements ) // will throw if we're empty!
    def relativize( other : T ) : UrlPath.Unrooted =
      val shared = this.elements.zip(other.elements).takeWhile(tup => tup(0) == tup(1)).map(_(0))
      Unrooted( Array.fill(elements.length - shared.length)("..").to(Vector) ++ other.elements )
    def dedottify : T =
      val dedot1 = elements.filter( _ == ".")
      if dedot1(0) == ".." then
        throw new CannotDedottify(s"Can't dedottify '${this}', can't resolve initial path element.")
      val offset = "DUMMY" +: dedot1
      val offsetSnipped = dedot1.zip(offset).filter( tup => tup(0) != ".." ).map( tup => tup(1) ).filter( _ != ".." )
      this.copy( offsetSnipped.tail )

    override def toString() : String = elements.map( URLEncoder.encode(_, scala.io.Codec.UTF8.charSet) ).mkString("/")
  end PathPart

  object Rooted:
    private def preparse( path : String ) : Vector[String] =
      if path.isEmpty || path(0) != '/' then
        throw new BadPathException(s"Putative rooted path '${path}' must begin with '/'.")
      path.split("""\/+""").filter(_.nonEmpty).to(Vector)

    object Server:
      def fromElements( elements : String* ) : Server = Server( elements.filter( _.nonEmpty ).to(Vector) )
      def parse( path : String )             : Server = Server( preparse(path) )
      def apply( path : String )             : Server = Server.parse(path)
    case class Server private[unstatic] ( val elements : Vector[String] ) extends Rooted[Server]

    object Site:
      def fromElements( elements : String* ) : Site = Site( elements.filter( _.nonEmpty ).to(Vector) )
      def parse( path : String )             : Site = Site( preparse(path) )
      def apply( path : String )             : Site = Site.parse(path)

    case class Site private[unstatic] ( val elements : Vector[String] ) extends Rooted[Site]

    object App:
      def fromElements( elements : String* ) : App = App( elements.filter( _.nonEmpty ).to(Vector) )
      def parse( path : String )             : App = App( preparse(path) )
      def apply( path : String )             : App = App.parse(path)
    case class App private[unstatic] ( val elements : Vector[String] ) extends Rooted[App]

  trait Rooted[T <: Rooted[T]] extends PathPart[T]:
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
  case class Unrooted private[unstatic](elements: Vector[String]) extends PathPart[Unrooted]

sealed trait UrlPath:
  def resolve(unrooted: UrlPath.Unrooted): UrlPath
  def resolveSibling(unrooted: UrlPath.Unrooted): UrlPath


