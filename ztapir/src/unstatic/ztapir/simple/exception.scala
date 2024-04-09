package unstatic.ztapir.simple

import unstatic.UnstaticException

class NoHtmlifierForContentType( msg : String, cause : Throwable = null ) extends UnstaticException(msg, null)