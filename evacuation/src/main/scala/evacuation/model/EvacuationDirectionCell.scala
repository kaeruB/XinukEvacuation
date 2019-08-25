package evacuation.model

import evacuation.config.EvacuationConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, Cell, EmptyCell, GridPart, Signal, SmellingCell}

final case class EvacuationDirectionCell(smell: SmellArray)(implicit config: EvacuationConfig) extends SmellingCell {
  override type Self = EvacuationDirectionCell
  override def withSmell(smell: SmellArray):EvacuationDirectionCell  = copy(smell = smell)
}

//object EvacuationDirectionCell {
//  def create(initialSignal: Signal): EvacuationDirectionCell = EvacuationDirectionCell(Array.fill(Cell.Size, Cell.Size)(initialSignal))
//}

trait EvacuationDirectionAccessible[+T <: GridPart] {
  def withEvacuationDirection(): T
}

object EvacuationDirectionAccessible {
  def unapply (arg: EmptyCell)(implicit config: EvacuationConfig): EvacuationDirectionAccessible[EvacuationDirectionCell] =
    new EvacuationDirectionAccessible[EvacuationDirectionCell] {
      override def withEvacuationDirection(): EvacuationDirectionCell = EvacuationDirectionCell(arg.smellWith(config.evacuationDirectionInitialSignal))
    }

  def unapply (arg: BufferCell)(implicit config: EvacuationConfig): EvacuationDirectionAccessible[BufferCell] =
    new EvacuationDirectionAccessible[BufferCell] {
      override def withEvacuationDirection(): BufferCell = BufferCell(EvacuationDirectionCell(arg.smellWith(config.evacuationDirectionInitialSignal)))
    }

  def unapply(arg: GridPart)(implicit config: EvacuationConfig): Option[EvacuationDirectionAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
