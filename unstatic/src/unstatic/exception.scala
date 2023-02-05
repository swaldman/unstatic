package unstatic

class UnstaticException( message : String, cause : Throwable = null) extends Exception(message,cause)
class BadPath( message : String, cause : Throwable = null)  extends UnstaticException(message,cause)
class CannotRelativize( message : String, cause : Throwable = null)  extends UnstaticException(message,cause)
class MissingAttribute( message : String, cause : Throwable = null)  extends UnstaticException(message,cause)
class NoSuchEntry(message : String, cause : Throwable = null)  extends UnstaticException(message,cause)
class BadCommandLine(message : String, cause : Throwable = null)  extends UnstaticException(message,cause)





