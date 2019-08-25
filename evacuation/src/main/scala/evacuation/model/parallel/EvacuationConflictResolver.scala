package evacuation.model.parallel

import evacuation.config.EvacuationConfig
import evacuation.model.WallCell
import evacuation.simulation.EvacuationMetrics
import pl.edu.agh.xinuk.model.{EmptyCell, GridPart, Obstacle, SmellingCell}
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object EvacuationConflictResolver extends ConflictResolver[EvacuationConfig] {
  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: EvacuationConfig): (GridPart, EvacuationMetrics) = {
    (current, incoming) match {
      case (WallCell(currentSmell), _) =>
        (WallCell(currentSmell), EvacuationMetrics.empty())
      case (Obstacle, _) =>
        (Obstacle, EvacuationMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}