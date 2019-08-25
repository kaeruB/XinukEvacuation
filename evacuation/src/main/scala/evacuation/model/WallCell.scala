package evacuation.model

import evacuation.config.EvacuationConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, Cell, EmptyCell, GridPart, Signal, SmellingCell}

// (implicit config: EvacuationConfig)
final case class WallCell(smell: SmellArray) extends SmellingCell {
  override type Self = WallCell
  override def withSmell(smell: SmellArray):WallCell  = copy(smell = smell)
}

object WallCell {
  def create(initialSignal: Signal): WallCell = WallCell(Array.fill(Cell.Size, Cell.Size)(initialSignal))
}

//trait WallAccessible[+T <: GridPart] {
//  def withWall(): T
//}
//
//object WallAccessible {
//  def unapply (arg: EmptyCell)(implicit config: EvacuationConfig): WallAccessible[WallCell] =
//    new WallAccessible[WallCell] {
//      override def withWall(): WallCell = WallCell(arg.smellWith(config.wallInitialSignal))
//    }
//
//  def unapply (arg: BufferCell)(implicit config: EvacuationConfig): WallAccessible[BufferCell] =
//    new WallAccessible[BufferCell] {
//      override def withWall(): BufferCell = BufferCell(WallCell(arg.smellWith(config.wallInitialSignal)))
//    }
//
//  def unapply(arg: GridPart)(implicit config: EvacuationConfig): Option[WallAccessible[GridPart]] = arg match {
//    case cell: EmptyCell => Some(unapply(cell))
//    case cell: BufferCell => Some(unapply(cell))
//    case _ => None
//  }
//}