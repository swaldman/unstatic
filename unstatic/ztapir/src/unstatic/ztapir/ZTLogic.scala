package unstatic.ztapir

import zio.*
import scala.collection.*

object ZTLogic:
  case class Generic[-I,+O]( val logic : Function1[I,Task[O]] ) extends ZTLogic[I,O]
  case class UnitString( val task : Task[String])  extends ZTLogic[Unit,String]:
    def logic : Function1[Unit,Task[String]] = (_ : Unit) => task
  case class UnitArraySeqByte( val task : Task[immutable.ArraySeq[Byte]] ) extends ZTLogic[Unit,immutable.ArraySeq[Byte]]:
    def logic : Function1[Unit,Task[immutable.ArraySeq[Byte]]] = (_ : Unit) => task
sealed trait ZTLogic[-I,+O]:
  def logic : Function1[I,Task[O]]
