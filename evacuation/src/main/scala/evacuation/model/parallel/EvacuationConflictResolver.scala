package evacuation.model.parallel

import evacuation.config.EvacuationConfig
import evacuation.model.{PersonCell, WallCell}
import evacuation.simulation.EvacuationMetrics
import pl.edu.agh.xinuk.model.{Cell, EmptyCell, GridPart, Obstacle, SmellingCell}
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object EvacuationConflictResolver extends ConflictResolver[EvacuationConfig] {
  import Cell._

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: EvacuationConfig): (GridPart, EvacuationMetrics) = {
    (current, incoming) match {
      case  (EmptyCell(currentSmell), EmptyCell(incomingSmell)) =>
        (EmptyCell(currentSmell + incomingSmell), EvacuationMetrics.empty())

      case (PersonCell(currentSmell, reachedCorridor), EmptyCell(incomingSmell)) =>
        (PersonCell(currentSmell + incomingSmell, reachedCorridor), EvacuationMetrics.empty())

      case (EmptyCell(incomingSmell), PersonCell(currentSmell, reachedCorridor)) =>
        (PersonCell(currentSmell + incomingSmell, reachedCorridor), EvacuationMetrics.empty())

      case (PersonCell(currentSmell, reachedCorridor), another@PersonCell(incomingSmell, incomingReachedCorridor)) =>
        (PersonCell(currentSmell + incomingSmell, reachedCorridor),  EvacuationMetrics.empty())

      case (WallCell(currentSmell), _) =>
        (WallCell(currentSmell), EvacuationMetrics.empty())

      case (Obstacle, _) =>
        (Obstacle, EvacuationMetrics.empty())

      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")

    }
  }
}