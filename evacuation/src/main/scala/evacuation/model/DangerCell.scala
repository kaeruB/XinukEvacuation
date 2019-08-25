package evacuation.model

import evacuation.config.EvacuationConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart, SmellingCell}

final case class DangerCell(smell: SmellArray)(implicit config: EvacuationConfig)  extends SmellingCell {
  override type Self = DangerCell
  override def withSmell(smell: SmellArray):DangerCell  = copy(smell = smell)
}

trait DangerAccessible[+T <: GridPart] {
  def withDanger(): T
}

object DangerAccessible {
  def unapply (arg: EmptyCell)(implicit config: EvacuationConfig): DangerAccessible[DangerCell] =
    new DangerAccessible[DangerCell] {
      override def withDanger(): DangerCell = DangerCell(arg.smellWith(config.dangerInitialSignal))
    }

  def unapply (arg: BufferCell)(implicit config: EvacuationConfig): DangerAccessible[BufferCell] =
    new DangerAccessible[BufferCell] {
      override def withDanger(): BufferCell = BufferCell(DangerCell(arg.smellWith(config.dangerInitialSignal)))
    }

  def unapply(arg: GridPart)(implicit config: EvacuationConfig): Option[DangerAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}