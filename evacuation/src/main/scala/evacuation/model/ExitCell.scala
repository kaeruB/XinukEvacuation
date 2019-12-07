package evacuation.model

import evacuation.config.EvacuationConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, EmptyCell, GridPart, Signal, SmellingCell}

//final case class ExitCell(smell: SmellArray) extends SmellingCell {
//  override type Self = ExitCell
//  override def withSmell(smell: SmellArray):ExitCell  = copy(smell = smell)
//}
//
//object ExitCell {
//  def create(initialSignal: Signal): ExitCell = ExitCell(Array.fill(Cell.Size, Cell.Size)(initialSignal))
//}

final case class ExitCell (id: Int, smell: SmellArray) extends SmellingCell {
  override type Self = ExitCell
  override def withSmell(smell: SmellArray):ExitCell = copy(smell = smell)
}

trait ExitCellAccessible[+T <: GridPart] {
  def withExitCell(id: Int): T
}

object ExitCellAccessible {
  def unapply(arg: EmptyCell)(implicit config: EvacuationConfig): ExitCellAccessible[ExitCell] =
    new ExitCellAccessible[ExitCell] {
      override def withExitCell(id: Int): ExitCell = ExitCell(id, Cell.emptySignal)
    }

  def unapply(arg: GridPart)(implicit config: EvacuationConfig): Option[ExitCellAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    // case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}