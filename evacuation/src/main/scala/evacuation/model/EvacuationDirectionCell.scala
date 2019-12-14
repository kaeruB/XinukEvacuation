package evacuation.model

import evacuation.model.EvacuationDirectionSmellStrength.EvacuationDirectionSmellStrength
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

final case class EvacuationDirectionCell(
                                          smell: SmellArray,
                                          exit: Boolean,
                                          evacuationDirectionSmellStrength: EvacuationDirectionSmellStrength
                                        ) extends SmellingCell{
  override type Self = EvacuationDirectionCell
  override def withSmell(smell: SmellArray): EvacuationDirectionCell = copy(smell = smell)
}

object EvacuationDirectionCell {
  def create(
              initialSignal: Signal,
              exit: Boolean,
              evacuationDirectionSmellStrength: EvacuationDirectionSmellStrength
            ): EvacuationDirectionCell =
    EvacuationDirectionCell(Array.fill(Cell.Size, Cell.Size)(initialSignal), exit, evacuationDirectionSmellStrength)
}