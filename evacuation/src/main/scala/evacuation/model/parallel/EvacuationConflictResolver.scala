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

      case (PersonCell(currentSmell, currentWaitingPeopleNo, currentIsInCorridor), EmptyCell(incomingSmell)) =>
        (PersonCell(currentSmell + incomingSmell, currentWaitingPeopleNo, currentIsInCorridor), EvacuationMetrics.empty())

      case (EmptyCell(incomingSmell), PersonCell(currentSmell, currentWaitingPeopleNo, currentIsInCorridor)) =>
        (PersonCell(currentSmell + incomingSmell, currentWaitingPeopleNo, currentIsInCorridor), EvacuationMetrics.empty())

      case (PersonCell(currentSmell, currentWaitingPeopleNo, currentIsInCorridor), another@PersonCell(incomingSmell, incomingCurrentWaitingPeopleNo, _)) =>
        (PersonCell(currentSmell + incomingSmell, currentWaitingPeopleNo ++ incomingCurrentWaitingPeopleNo, currentIsInCorridor),  EvacuationMetrics.empty())

      case (WallCell(currentSmell), _) =>
        (WallCell(currentSmell), EvacuationMetrics.empty())

      case (Obstacle, _) =>
        (Obstacle, EvacuationMetrics.empty())

      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")

    }
  }
}