package evacuation.model

import evacuation.config.EvacuationConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{GridPart, SmellingCell}

final case class ExitCell (id: Int, smell: SmellArray) extends SmellingCell {
  override type Self = ExitCell
  override def withSmell(smell: SmellArray):ExitCell = copy(smell = smell)
}

trait ExitCellAccessible[+T <: GridPart] {
  def withExitCell(id: Int): T
}

object ExitCellAccessible {

  def unapply(arg: GridPart)(implicit config: EvacuationConfig): Option[ExitCellAccessible[GridPart]] = arg match {
    case _ => None
  }
}