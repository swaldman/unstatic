package unstatic.ztapir

import zio.*

object ZTLogic:
  case class Generic[-I,+O]( val logic : Function1[I,Task[O]] ) extends ZTLogic[I,O]
  case class UnitString( val task : Task[String])  extends ZTLogic[Unit,String]:
    def logic : Function1[Unit,Task[String]] = (_ : Unit) => task
sealed trait ZTLogic[-I,+O]:
  def logic : Function1[I,Task[O]]
