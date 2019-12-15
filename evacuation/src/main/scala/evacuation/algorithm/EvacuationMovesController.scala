package evacuation.algorithm

import com.avsystem.commons.misc.Opt
import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import evacuation.config.EvacuationConfig
import evacuation.model.EvacuationDirectionSmellStrength.EvacuationDirectionSmellStrength
import evacuation.model.building.BuildingMap
import evacuation.model.{EvacuationDirectionCell, EvacuationDirectionSmellStrength, ExitCell, ExitCellAccessible, PersonAccessible, PersonCell, TeleportationAccessible, TeleportationCell}
import evacuation.simulation.EvacuationMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, EmptyCell, Grid, GridPart, Obstacle, Signal}

import scala.collection.immutable.TreeSet

final class EvacuationMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: EvacuationConfig)  extends MovesController {

  val boundaryIterationNo = 16
  val createExitsIterationNo = 7
  var staticSmellFloor: Array[Array[SmellArray]] = Array.ofDim[SmellArray](config.gridSize, config.gridSize)
  val buildingMap: BuildingMap = new BuildingMap()

  override def initialGrid: (Grid, EvacuationMetrics) = {
    val grid = Grid.empty(bufferZone)
    val metrics = EvacuationMetrics(0)

    val wallsPoints = buildingMap.wallsPoints

    def createCells(): Unit = {

      def createWallCells(): Unit = {
        for (point <- wallsPoints) {
          grid.cells(point.y)(point.x) = Obstacle
        }
      }

      def createSmellSources(): Unit = {
        for (smellOrigin <- buildingMap.smellOrigins) {
          grid.cells(smellOrigin.y)(smellOrigin.x) =
            EvacuationDirectionCell.create(config.evacuationDirectionInitialSignal, false, EvacuationDirectionSmellStrength.Strong)
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
    var evacuatedCount = 0L

    def propagateInitialSmell(): Unit = {
      val (dynamicCells, staticCells) = (for {
        y <- 0 until config.gridSize
        x <- 0 until config.gridSize
      } yield (x, y, grid.cells(x)(y))).partition({
        case (_, _, EvacuationDirectionCell(_, _, _)) => true
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

    def copyEvacuationDirectionCell(x: Int, y: Int, cell: EvacuationDirectionCell): Unit = {
      val strength: (Signal, EvacuationDirectionSmellStrength) = cell match {
        case EvacuationDirectionCell(_, _, EvacuationDirectionSmellStrength.Weak) =>
          (config.evacuationDirectionInitialSignalWeak, EvacuationDirectionSmellStrength.Weak)
        case EvacuationDirectionCell(_, _, EvacuationDirectionSmellStrength.Medium) =>
          (config.evacuationDirectionInitialSignalMedium, EvacuationDirectionSmellStrength.Medium)
        case EvacuationDirectionCell(_, _, EvacuationDirectionSmellStrength.Strong) =>
          (config.evacuationDirectionInitialSignal, EvacuationDirectionSmellStrength.Strong)
        case _ => null
      }
      newGrid.cells(x)(y) = EvacuationDirectionCell.create(strength._1, cell.exit, strength._2)
    }

    def createSmellSnapshot(): Unit = {
      for {
        x <- 0 until config.gridSize
        y <- 0 until config.gridSize
      } grid.cells(y)(x) match {
        case EvacuationDirectionCell(_, exit, _) => {
          if (!exit) {
            val id = buildingMap.teleportationPairs.indexWhere(teleportPair => teleportPair.point1.x == x && teleportPair.point1.y == y)
            newGrid.cells(y)(x) = TeleportationCell(id, Cell.emptySignal)
          }
          else {
            newGrid.cells(y)(x) = ExitCell(0, Cell.emptySignal) // TODO id
          }
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
      for (point <- buildingMap.peoplePointsOnFloor1) {
        newGrid.cells(point.y)(point.x) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      }
    }

    def placePeopleOnGridTest(): Unit = {
      newGrid.cells(129)(21) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      newGrid.cells(129)(23) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
    }

    def simulateEvacuation(): Unit = {
      val (dynamicCells, staticCells) = (for {
        y <- 0 until config.gridSize
        x <- 0 until config.gridSize
      } yield (x, y, grid.cells(x)(y))).partition({
        case (_, _, Obstacle) => true
        case (_, _, TeleportationCell(_, _)) => false
        case (_, _, ExitCell(_, _)) => false
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

    def movePersonCell(cellY: Int, cellX: Int, cell: PersonCell): Unit = {
      val destinations = calculatePossibleDestinations(cell, cellY, cellX, grid)
      val destination = selectDestinationCell(destinations, newGrid)

      destination match {
        case Opt((i, j, PersonAccessible(destination))) => {
          grid.cells(i)(j) match {

            case TeleportationCell(id, _) => {
              if (id != -1)
                newGrid.cells(buildingMap.teleportationPairs(id).point2.y)(buildingMap.teleportationPairs(id).point2.x) = cell
            }

            case ExitCell(_, _) => {
              evacuatedCount += 1
            }

            case _ => {
              newGrid.cells(i)(j) = destination.withPerson()
            }
          }

          // do not remove a person behind
          if (cell == newGrid.cells(cellY)(cellX))
            newGrid.cells(cellY)(cellX) = EmptyCell(cell.smell)
        }

        case Opt((i, j, inaccessibleDestination)) =>
          inaccessibleDestination match {
            case _ => {
              throw new RuntimeException(s"Person selected inaccessible destination ($i,$j): $inaccessibleDestination")
            }
          }
        case Opt.Empty =>
          newGrid.cells(cellY)(cellX) = cell
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
          case (i, j, currentCell@PersonAccessible(_), ExitCellAccessible(_)) =>
            (i, j, currentCell)
          case (i, j, currentCell@PersonAccessible(_), PersonAccessible(_)) =>
            (i, j, currentCell)
          case (i, j, currentCell@PersonAccessible(_), TeleportationAccessible(_)) =>
            (i, j, currentCell)
        }
    }

    def createExitCells(): Unit= {
      for (smellOrigin <- buildingMap.exits) {
          val signal: Signal = smellOrigin._2 match {
            case EvacuationDirectionSmellStrength.Strong => config.evacuationDirectionInitialSignal
            case EvacuationDirectionSmellStrength.Medium => config.evacuationDirectionInitialSignalMedium
            case EvacuationDirectionSmellStrength.Weak => config.evacuationDirectionInitialSignalWeak
            case _ => config.evacuationDirectionInitialSignal
          }
          grid.cells(smellOrigin._1.y)(smellOrigin._1.x) =
            EvacuationDirectionCell.create(signal, true, smellOrigin._2)
        }
    }

    if (iteration < boundaryIterationNo) {
      if (iteration == createExitsIterationNo)
        createExitCells()
      propagateInitialSmell()
    }
    else if (iteration == boundaryIterationNo) {
      createSmellSnapshot()
      placePeopleOnGrid()
      // placePeopleOnGridTest()
    }
    else simulateEvacuation()

    val metrics = EvacuationMetrics(evacuatedCount)
    (newGrid, metrics)
  }
}

