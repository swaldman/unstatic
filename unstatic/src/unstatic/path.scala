package unstatic

import scala.language.strictEquality

given CanEqual[Path, Path] = CanEqual.derived

trait Path:
  def asString : String
  def resolve( relPath : RelPath ) : Path
  override def toString() = asString
  override def hashCode() = asString.hashCode()
  override def equals( o : Any ) : Boolean = o.isInstanceOf[Path] && o.asInstanceOf[Path].asString.equals(this.asString)

object AbsPath:
  private final case class Default( val asString : String ) extends AbsPath:
    require (isAbsolute(asString), s"'${asString}' is not an absolute URL path")
    def resolve( relPath : RelPath ) : AbsPath = AbsPath.Default(_pathJoin( this.asString, relPath.asString ))
    def relativize( absPath : AbsPath ) : RelPath = RelPath(_relativize(this.asString, absPath.asString))
  def apply( s : String ) : AbsPath = Default(s)
trait AbsPath extends Path:
  def resolve( relPath : RelPath ) : AbsPath
  def relativize( absPath : AbsPath ) : RelPath

object RelPath:
  private final case class Default( val asString : String ) extends RelPath:
    require (!isAbsolute(asString), s"'${asString}' is not a relative URL path")
    def resolve(relPath: RelPath): RelPath = RelPath.Default(_pathJoin(this.asString, relPath.asString))
    def relativize( relPath : RelPath ) : RelPath = RelPath(_relativize(this.asString, relPath.asString))
  def apply( s : String ) : RelPath = Default(s)
trait RelPath extends Path:
  def resolve(relPath: RelPath): RelPath
  def relativize( relPath : RelPath ) : RelPath

// TODO: Better validation that "absolute" paths are
//       valid absolute URLs
private def isAbsolute( pathString : String ) : Boolean =
  val colonIndex = pathString.indexOf(':')
  val slashIndex = pathString.indexOf('/')
  ( colonIndex, slashIndex ) match
    case (-1, -1) => false
    case ( _, -1) => true
    case (-1, _ ) => false
    case (ci, si) => ci < si

private def _pathJoinBinary( elem0 : String, elem1 : String) : String =
  if (elem0.isEmpty || elem1.isEmpty)
    elem0 + elem1
  else
    (elem0.last, elem1.head) match
      case ('/', '/') => elem0 + elem1.substring(1)
      case ('/',  _ ) => elem0 + elem1
      case ( _ , '/') => elem0 + elem1
      case ( _ ,  _ ) => elem0 + '/' + elem1

private def _pathJoin( elem0 : String, elems : String*) : String =
  elems.foldLeft( elem0 )( (accum, next) => _pathJoinBinary(accum, next) )

private def _relativize( basePath : String, target : String) : String =
  val shared = basePath.zip(target).takeWhile( tup => tup(0) == tup(1) )
  val sharedLength = shared.size
  if sharedLength == 0 then
    throw new CannotRelativize(s"Base path '${basePath}' and target path '${target}' share no prefix")
  else if (basePath.length <= target.length)
    val basePathDir = _asDirOrGetParent(basePath.substring(0,sharedLength))
    target.substring(basePathDir.length)
  else
    // http://localhost/dir1/dir2/dir3/name base
    // http://localhost/dir1/name2          target
    // ../../name2
    val targetPathDir  = _asDirOrGetParent(target)
    val targetPathName = target.substring(targetPathDir.length)
    val basePathExtra  = basePath.substring(targetPathDir.length)
    val extraSlashes = basePathExtra.filter( _ == '/').length
    ("../" * extraSlashes) + targetPathName

private val UrlStartRegex = """^(\w+)\:\/+$""".r

private def _asDirOrGetParent( path : String ) : String =
  val lastSlash = path.lastIndexOf('/')
  val LastIndex = path.length - 1
  val out =
    lastSlash match
      case LastIndex => path
      case -1        => ""
      case _         => path.substring(0, lastSlash+1) // include the slash
  out match
    case UrlStartRegex( scheme ) =>
      if scheme != "file" then
        path + "/" // we were looking for the parent of a non-slash terminated URL
      else
        out
    case _ => out


