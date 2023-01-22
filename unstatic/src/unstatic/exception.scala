package unstatic

class UnstaticException( message : String, cause : Throwable = null) extends Exception(message,cause)
class BadPathException( message : String, cause : Throwable = null) extends UnstaticException(message,cause)
class CannotRelativize( message : String, cause : Throwable = null) extends UnstaticException(message,cause)
class CannotDedottify( message : String, cause : Throwable = null) extends UnstaticException(message,cause)





