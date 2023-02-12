package unstatic.ztapir

import unstatic.UnstaticException

class UnresolvedReference( val source : String, val reference : String, val explanation : String, val absolute : Option[String] = None, cause : Throwable = null ) extends UnstaticException(s"$source: Unresolved reference: '${reference}', $explanation",cause)
