package unstatic.ztapir

import unstatic.UnstaticException

class UnresolvedReference( val source : String, val reference : String, val explanation : String, val absolute : Option[String] = None, cause : Throwable = null ) extends UnstaticException(s"$source: Unresolved reference: '${reference}', $explanation",cause)
class UnexpectedStaticLocationType(msg : String, cause : Throwable = null) extends UnstaticException(msg,cause)
class NotYetSupported(msg : String, cause : Throwable = null) extends UnstaticException(msg,cause)
class CantGuessImageType(msg : String, cause : Throwable = null) extends UnstaticException(msg,cause)
class IncludesUngenerableBindings(msg : String, cause : Throwable = null) extends UnstaticException(msg,cause)
class BadMediaType(msg : String, cause : Throwable = null) extends UnstaticException(msg,cause)
class BadGitRepository(msg : String, cause : Throwable = null) extends UnstaticException(msg,cause)
class GitResourceNotFound(msg : String, cause : Throwable = null) extends UnstaticException(msg,cause)
