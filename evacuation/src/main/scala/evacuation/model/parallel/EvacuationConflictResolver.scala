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

      case (PersonCell(currentSmell), EmptyCell(incomingSmell)) =>
        (PersonCell(currentSmell + incomingSmell), EvacuationMetrics.empty())

      case (EmptyCell(incomingSmell), PersonCell(currentSmell)) =>
        (PersonCell(currentSmell + incomingSmell), EvacuationMetrics.empty())

      case  (EmptyCell(currentSmell), EmptyCell(incomingSmell)) =>
        (EmptyCell(currentSmell + incomingSmell), EvacuationMetrics.empty())

      case (EmptyCell(currentSmell), incomingCell) =>
        (incomingCell.withSmell(incomingCell.smell + currentSmell), EvacuationMetrics.empty())

      case (currentCell: SmellingCell, EmptyCell(incomingSmell)) =>
        (currentCell.withSmell(currentCell.smell + incomingSmell), EvacuationMetrics.empty())

      // TODO
      case (PersonCell(currentSmell), another@PersonCell(incomingSmell)) =>
        (PersonCell(currentSmell + incomingSmell),  EvacuationMetrics.empty())

      case (WallCell(currentSmell), _) =>
        (WallCell(currentSmell), EvacuationMetrics.empty())

      case (Obstacle, _) =>
        (Obstacle, EvacuationMetrics.empty())

      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")

    }
  }
}