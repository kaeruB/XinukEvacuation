package evacuation.model

import evacuation.config.EvacuationConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.SmellingCell

final case class StaircaseCell(smell: SmellArray)(implicit config: EvacuationConfig) extends SmellingCell {
  override type Self = StaircaseCell
  override def withSmell(smell: SmellArray):StaircaseCell  = copy(smell = smell)
}

object StaircaseCell {
  // def create(initialSignal: )
}