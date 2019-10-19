package evacuation.algorithm

import com.avsystem.commons.misc.Opt
import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import evacuation.config.EvacuationConfig
import evacuation.model.{EvacuationDirectionAccessible, EvacuationDirectionCell, PersonAccessible, PersonCell}
import evacuation.simulation.EvacuationMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{ EmptyCell, Grid, GridPart, Obstacle, Signal}

import scala.collection.immutable.TreeSet

//    grid.cells(y)(x).smell -> SmellArray  ((Signal, Signal, Signal), (Signal, Signal, Signal), (Signal, Signal, Signal))
//    grid.cells(y)(x).smell(y2 = 0, 1, 2)(x2 = 0, 1, 2) -> Signal, float number

final class EvacuationMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: EvacuationConfig)  extends MovesController {

  val boundaryIterationNo = 40
  var staticSmellFloor: Array[Array[SmellArray]] = Array.ofDim[SmellArray](config.gridSize, config.gridSize)

  override def initialGrid: (Grid, EvacuationMetrics) = {
    val grid = Grid.empty(bufferZone)
    val metrics = EvacuationMetrics()

    def oneFloor(): Unit = {
      for (x <- 40 to 100) {
        grid.cells(40)(x) = Obstacle
        grid.cells(100)(x) = Obstacle
      }

      for (x <- 60 to 70) {
        grid.cells(60)(x) = Obstacle
        grid.cells(70)(x) = Obstacle
      }

      for (y <- 40 to 100) {
        grid.cells(y)(40) = Obstacle
        grid.cells(y)(100) = Obstacle
      }

      for (y <- 60 to 70) {
        grid.cells(y)(60) = Obstacle
        grid.cells(y)(70) = Obstacle
      }

      grid.cells(65)(60) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection()
      grid.cells(65)(70) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection()
    }

    def bendedCorridor(): Unit = {
      val halfOfGridSize = config.gridSize / 2

      for (x <- 0 until halfOfGridSize) {
        grid.cells(halfOfGridSize - 1)(x) = Obstacle
        grid.cells(halfOfGridSize + 1)(x) = Obstacle
      }
      grid.cells(halfOfGridSize + 1)(halfOfGridSize) = Obstacle
      grid.cells(halfOfGridSize + 1)(halfOfGridSize + 1) = Obstacle

      for (y <- 0 until halfOfGridSize) {
        grid.cells(y)(halfOfGridSize - 1) = Obstacle
        grid.cells(y)(halfOfGridSize + 1) = Obstacle
      }
      grid.cells(halfOfGridSize)(halfOfGridSize + 1) = Obstacle
      grid.cells(halfOfGridSize + 1)(halfOfGridSize + 1) = Obstacle

      grid.cells(halfOfGridSize)(0) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection()
    }

    // oneFloor()
    bendedCorridor()

    (grid, metrics)
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, EvacuationMetrics) = {
    val newGrid = Grid.empty(bufferZone)

    def propagateInitialSmell(): Unit = {
      val (dynamicCells, staticCells) = (for {
        y <- 0 until config.gridSize
        x <- 0 until config.gridSize
      } yield (x, y, grid.cells(x)(y))).partition({
        case (_, _, EvacuationDirectionCell(_)) => true
        case (_, _, _) => false
      })

      staticCells.foreach({
        case (x, y, cell) => copyCells(x, y, cell)
      })

      dynamicCells.foreach({
        case (x, y, cell: EvacuationDirectionCell) => copyEvacuationDirectionCell(x, y, cell)
        case (_, _, _) =>
      })
    }

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def copyEvacuationDirectionCell(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection() //EvacuationDirectionCell.create(config.evacuationDirectionInitialSignal)
    }

    def createSmellSnapshot(): Unit = {
      propagateInitialSmell()
      for {
              y <- 0 until config.gridSize
              x <- 0 until config.gridSize
      } newGrid.cells(x)(y) match {
        case EvacuationDirectionCell(_) => {
          newGrid.cells(x)(y) = EmptyCell(newGrid.cells(x)(y).smell)
          staticSmellFloor(x)(y) = newGrid.cells(y)(x).smell
        }
        case _ => staticSmellFloor(x)(y) = newGrid.cells(y)(x).smell
      }
    }

    def placePeopleOnGrid(): Unit = {
      newGrid.cells(70)(70) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      newGrid.cells(50)(50) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
    }

    def simulateEvacuation(): Unit = {
      val (dynamicCells, staticCells) = (for {
        y <- 0 until config.gridSize
        x <- 0 until config.gridSize
      } yield (x, y, grid.cells(x)(y))).partition({
        case (_, _, Obstacle) => true
        case (_, _, EvacuationDirectionCell(_)) => true
        case (_, _, PersonCell(_)) => true
        case (_, _, _) => false
      })


      staticCells.foreach({
        case (x, y, cell) => copyCells(x, y, cell)
      })

      dynamicCells.foreach({
        case (x, y, cell: PersonCell) => movePersonCell(x, y, cell)
        case (x, y, cell: EvacuationDirectionCell) => copyEvacuationDirectionCell(x, y, cell)
        case (x, y, Obstacle) => newGrid.cells(x)(y) = Obstacle
        case (_, _, _) =>
      })
    }

    def movePersonCell(x: Int, y: Int, cell: PersonCell): Unit = {
      val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)

      destination match {
        case Opt((i, j, PersonAccessible(destination))) =>
          newGrid.cells(i)(j) = destination.withPerson()
          newGrid.cells(x)(y) = EmptyCell(cell.smell)
        case Opt((i, j, inaccessibleDestination)) =>
          throw new RuntimeException(s"Person selected inaccessible destination ($i,$j): $inaccessibleDestination")
        case Opt.Empty =>
          newGrid.cells(x)(y) = cell.copy(cell.smell)
      }
    }

    def calculatePossibleDestinations(cell: PersonCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
      val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
//      Grid.SubcellCoordinates - Vector 8: (0 (0,0), 1 (0,1), 2 (0,2), 3 (1,0), ...)
//      TEST:
//      val tmp0 = Grid.SubcellCoordinates
//        .map { case (i, j) => cell.smell(i)(j) } // + staticSmellFloor(x)(y)(i)(j)
//        .zipWithIndex
//        .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
//
//      val tmp = Grid.SubcellCoordinates
//        .map { case (i, j) => cell.smell(i)(j) + staticSmellFloor(x)(y)(i)(j) } // + staticSmellFloor(x)(y)(i)(j)
//        .zipWithIndex
//        .sorted(implicitly[Ordering[(Signal, Int)]].reverse)

      Grid.SubcellCoordinates
        .map { case (i, j) => cell.smell(i)(j) + staticSmellFloor(x)(y)(i)(j) }
        .zipWithIndex
        .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
        .iterator
        .map { case (_, idx) =>
          val (i, j) = neighbourCellCoordinates(idx)
          (i, j,  grid.cells(i)(j))
        }
    }

    def selectDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): commons.Opt[(Int, Int, GridPart)] = {
      possibleDestinations
        .map { case (i, j, current) => (i, j, current, newGrid.cells(i)(j)) }
        .collectFirstOpt {
          case (i, j, currentCell@PersonAccessible(_), PersonAccessible(_)) =>
            (i, j, currentCell)
          case (i, j, currentCell@PersonAccessible(_), EvacuationDirectionAccessible(_)) =>
            (i, j, currentCell)
        }
    }

    if (iteration < boundaryIterationNo)
      propagateInitialSmell()
    else if (iteration == boundaryIterationNo) {
      createSmellSnapshot()
      placePeopleOnGrid()
    } else
      simulateEvacuation()

    val metrics = EvacuationMetrics()
    (newGrid, metrics)
  }
}

