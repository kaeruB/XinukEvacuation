package evacuation.algorithm

import com.avsystem.commons.misc.Opt
import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import evacuation.config.EvacuationConfig
import evacuation.model.{BuildingMap, EvacuationDirectionAccessible, EvacuationDirectionCell, PersonAccessible, PersonCell, Point, TeleportationAccessible, TeleportationCell, TeleportationPair}
import evacuation.simulation.EvacuationMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, EmptyCell, Grid, GridPart, Obstacle, Signal}

import scala.collection.immutable.TreeSet

final class EvacuationMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: EvacuationConfig)  extends MovesController {

  val boundaryIterationNo = 30
  var staticSmellFloor: Array[Array[SmellArray]] = Array.ofDim[SmellArray](config.gridSize, config.gridSize)
  val buildingMap: BuildingMap = new BuildingMap()

  override def initialGrid: (Grid, EvacuationMetrics) = {
    val grid = Grid.empty(bufferZone)
    val metrics = EvacuationMetrics()

    val wallsPoints = buildingMap.wallsPoints

    def createCells(): Unit = {

      def createWallCells(): Unit = {
        for (point <- wallsPoints) {
          grid.cells(point.y)(point.x) = Obstacle
        }
      }

      def createSmellSources(): Unit = {
        for (smellOrigin <- buildingMap.smellOrigins) {
          grid.cells(smellOrigin.y)(smellOrigin.x) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection()
        }
      }

      def createDoors(): Unit = {
        for (door <- buildingMap.doorsPoints) {
          grid.cells(door.y)(door.x) = EmptyCell(Cell.emptySignal)
        }
      }

      createWallCells()
      createSmellSources()
      createDoors()
    }

    createCells()

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
      for {
        x <- 0 until config.gridSize
        y <- 0 until config.gridSize
      } grid.cells(y)(x) match {
        case EvacuationDirectionCell(_) => {
          val id = buildingMap.teleportationPairs.indexWhere(teleportPair => teleportPair.point1.x == x && teleportPair.point1.y == y)
          newGrid.cells(y)(x) = TeleportationCell(id, Cell.emptySignal)  // EmptyCell(Cell.emptySignal)
          staticSmellFloor(y)(x) = grid.cells(y)(x).smell
        }
        case Obstacle => {
            newGrid.cells(y)(x) = grid.cells(y)(x)
        }
        case _ => {
          staticSmellFloor(y)(x) = grid.cells(y)(x).smell
        }
      }
    }

    def placePeopleOnGrid(): Unit = {
      for (point <- buildingMap.peoplePoints) {
        newGrid.cells(point.y)(point.x) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      }
    }

    def placePeopleOnGridTest(): Unit = {
      newGrid.cells(19)(54) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      newGrid.cells(19)(56) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
    }

    def simulateEvacuation(): Unit = {
      val (dynamicCells, staticCells) = (for {
        y <- 0 until config.gridSize
        x <- 0 until config.gridSize
      } yield (x, y, grid.cells(x)(y))).partition({
        case (_, _, Obstacle) => true
        case (_, _, TeleportationCell(_, _)) => false
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
      val doorPointWithAssociatedPointOnCorridor: Option[(Point, Point, Point)] =
        buildingMap.doorsPointsWithAssociatedPointsOnCorridor.find(points => points._1.x == x && points._1.y == y)

      if (doorPointWithAssociatedPointOnCorridor.nonEmpty) {
        val associatedCellStateOnGrid: GridPart = grid.cells(doorPointWithAssociatedPointOnCorridor.get._3.y)(doorPointWithAssociatedPointOnCorridor.get._3.x)

        associatedCellStateOnGrid match {
          case (cell: PersonCell) => {
            newGrid.cells(x)(y) = cell
          }
          case (cell: EmptyCell) => {
            newGrid.cells(doorPointWithAssociatedPointOnCorridor.get._2.y)(doorPointWithAssociatedPointOnCorridor.get._2.x) = cell
          }
        }
      }
      else {
        val destination = selectDestinationCell(destinations, newGrid)

        destination match {
          case Opt((i, j, PersonAccessible(destination))) => {
            grid.cells(i)(j) match {
              case TeleportationCell(id, _) => {
                if (id != -1)
                  newGrid.cells(buildingMap.teleportationPairs(id).point2.y)(buildingMap.teleportationPairs(id).point2.x) = cell.copy(cell.smell)
              }
              case _ => {
                newGrid.cells(i)(j) = destination.withPerson()
              }
            }
            newGrid.cells(x)(y) = EmptyCell(cell.smell)
          }
          case Opt((i, j, inaccessibleDestination)) =>
            throw new RuntimeException(s"Person selected inaccessible destination ($i,$j): $inaccessibleDestination")
          case Opt.Empty =>
            newGrid.cells(x)(y) = cell.copy(cell.smell)
        }
      }
    }

    def calculatePossibleDestinations(cell: PersonCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
      val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)

      Grid.SubcellCoordinates
        .map { case (i, j) => cell.smell(i)(j) + staticSmellFloor(x)(y)(i)(j)}
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
//          case (i, j, currentCell@PersonAccessible(_), EvacuationDirectionAccessible(_)) =>
//            (i, j, currentCell)
          case (i, j, currentCell@PersonAccessible(_), TeleportationAccessible(_)) =>
            (i, j, currentCell)
        }
    }

    if (iteration < boundaryIterationNo) propagateInitialSmell()
    else if (iteration == boundaryIterationNo) {
      createSmellSnapshot()
      // placePeopleOnGrid()
      placePeopleOnGridTest()
    }
    else simulateEvacuation()

    val metrics = EvacuationMetrics()
    (newGrid, metrics)
  }
}

