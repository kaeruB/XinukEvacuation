package evacuation.model

import evacuation.config.EvacuationConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, Cell, EmptyCell, GridPart, SmellingCell}

final case class TeleportationCell (id: Int, smell: SmellArray) extends SmellingCell {
  override type Self = TeleportationCell
  override def withSmell(smell: SmellArray):TeleportationCell = copy(smell = smell)
}

trait TeleportationAccessible[+T <: GridPart] {
  def withTeleportation(id: Int): T
}

object TeleportationAccessible {
  def unapply(arg: EmptyCell)(implicit config: EvacuationConfig): TeleportationAccessible[TeleportationCell] =
    new TeleportationAccessible[TeleportationCell] {
      override def withTeleportation(id: Int): TeleportationCell = TeleportationCell(id, Cell.emptySignal)
    }

  def unapply(arg: GridPart)(implicit config: EvacuationConfig): Option[TeleportationAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    // case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}