package evacuation.model

import evacuation.config.EvacuationConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart, SmellingCell}

final case class PersonCell(smell: SmellArray)(implicit config: EvacuationConfig)  extends SmellingCell {
  override type Self = PersonCell
  override def withSmell(smell: SmellArray): PersonCell = copy(smell = smell)
}

trait PersonAccessible[+T <: GridPart] {
  def withPerson(): T
}

object PersonAccessible {
  def unapply (arg: EmptyCell)(implicit config: EvacuationConfig): PersonAccessible[PersonCell] =
    new PersonAccessible[PersonCell] {
      override def withPerson(): PersonCell = PersonCell(arg.smellWith(config.personInitialSignal))
    }

  def unapply (arg: EvacuationDirectionCell)(implicit config: EvacuationConfig): PersonAccessible[EvacuationDirectionCell] =
    new PersonAccessible[EvacuationDirectionCell] {
      override def withPerson(): EvacuationDirectionCell = EvacuationDirectionCell(arg.smell)
    }


  def unapply (arg: BufferCell)(implicit config: EvacuationConfig): PersonAccessible[BufferCell] =
    new PersonAccessible[BufferCell] {
      override def withPerson(): BufferCell = BufferCell(PersonCell(arg.smellWith(config.personInitialSignal)))
    }

  def unapply(arg: GridPart)(implicit config: EvacuationConfig): Option[PersonAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: EvacuationDirectionCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}