package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.model.{Grid, WorkerId}
import pl.edu.agh.xinuk.simulation.Metrics

trait MovesController {
  def initialGrid(id: WorkerId): (Grid, Metrics)

  def makeMoves(iteration: Long, grid: Grid, id: WorkerId): (Grid, Metrics)
}
