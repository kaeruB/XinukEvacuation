package evacuation.algorithm

import com.avsystem.commons.misc.Opt
import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import evacuation.config.EvacuationConfig
import evacuation.model.{BuildingMap, EvacuationDirectionAccessible, EvacuationDirectionCell, ExitCell, ExitCellAccessible, PersonAccessible, PersonCell, Point, TeleportationAccessible, TeleportationCell, TeleportationPair}
import evacuation.simulation.EvacuationMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, EmptyCell, Grid, GridPart, Obstacle, Signal}

import scala.collection.immutable.TreeSet

final class EvacuationMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: EvacuationConfig)  extends MovesController {

  val boundaryIterationNo = 50
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
          grid.cells(smellOrigin.y)(smellOrigin.x) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection(false)
        }

        for (smellOrigin <- buildingMap.exits) {
          grid.cells(smellOrigin.y)(smellOrigin.x) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection(true)
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
        case (_, _, EvacuationDirectionCell(_, _)) => true
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

//    def copyEvacuationDirectionCell(x: Int, y: Int, cell: GridPart): Unit = {
//      newGrid.cells(x)(y) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection() //EvacuationDirectionCell.create(config.evacuationDirectionInitialSignal)
//    }

    def copyEvacuationDirectionCell(x: Int, y: Int, cell: EvacuationDirectionCell): Unit = {
      newGrid.cells(x)(y) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection(cell.exit) //EvacuationDirectionCell.create(config.evacuationDirectionInitialSignal)
    }

    def createSmellSnapshot(): Unit = {
      for {
        x <- 0 until config.gridSize
        y <- 0 until config.gridSize
      } grid.cells(y)(x) match {
        case EvacuationDirectionCell(_, exit) => {
          if (!exit) {
            val id = buildingMap.teleportationPairs.indexWhere(teleportPair => teleportPair.point1.x == x && teleportPair.point1.y == y)
            newGrid.cells(y)(x) = TeleportationCell(id, Cell.emptySignal) // EmptyCell(Cell.emptySignal)
          }
          else {
            newGrid.cells(y)(x) = ExitCell(0, Cell.emptySignal) //config.zeroInitialSignal // TODO id
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
        newGrid.cells(point.y)(point.x) = PersonAccessible.unapply(EmptyCell.Instance).withPerson(List.empty, false)
      }
    }

    def placePeopleOnGridTest(): Unit = {
      newGrid.cells(19)(54) = PersonAccessible.unapply(EmptyCell.Instance).withPerson(List.empty, false)
      newGrid.cells(19)(56) = PersonAccessible.unapply(EmptyCell.Instance).withPerson(List.empty, false)
    }

    def simulateEvacuation(): Unit = {
      val (dynamicCells, staticCells) = (for {
        y <- 0 until config.gridSize
        x <- 0 until config.gridSize
      } yield (x, y, grid.cells(x)(y))).partition({
        case (_, _, Obstacle) => true
        case (_, _, TeleportationCell(_, _)) => false
        case (_, _, ExitCell(_, _)) => false
        case (_, _, PersonCell(_, _, _)) => true
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
      val cellToMove: PersonCell = getCellWithUpdatedState(cellY, cellX, cell)

      val doorPointWithAssociatedPointOnCorridor: Option[(Point, Point, Point)] =
        buildingMap.doorsPointsWithAssociatedPointsOnCorridor.find(points => points._1.y == cellY && points._1.x == cellX)

      // going to corridor
      if (doorPointWithAssociatedPointOnCorridor.nonEmpty) { // try to enter corridor
        val associatedCellStateOnGrid: GridPart =
          grid.cells(doorPointWithAssociatedPointOnCorridor.get._3.y)(doorPointWithAssociatedPointOnCorridor.get._3.x)

        associatedCellStateOnGrid match {
          case (_: PersonCell) => {
            // stay
            newGrid.cells(cellY)(cellX) = cellToMove
          }
          case (_: EmptyCell) => {
            // enter corridor
            // if there are more than one people waiting - one person leaves, the others stay
            if (cellToMove.waitingPeople.nonEmpty) {
              newGrid.cells(doorPointWithAssociatedPointOnCorridor.get._2.y)(doorPointWithAssociatedPointOnCorridor.get._2.x) =
                cellToMove.copy(cellToMove.smell, cellToMove.waitingPeople.head.waitingPeople, true)
              newGrid.cells(cellY)(cellX) =  cellToMove.copy(cellToMove.waitingPeople.head.smell, cellToMove.waitingPeople.drop(1), false)
            }
              // if there is one person waiting - it will enter the corridor
            else {
              newGrid.cells(doorPointWithAssociatedPointOnCorridor.get._2.y)(doorPointWithAssociatedPointOnCorridor.get._2.x) =
                cellToMove.copy(cellToMove.smell, cellToMove.waitingPeople, true)
            }
          }
        }
      }
      else {
        val doorDestinationOpt: Option[((Int, Int), Point)] =
          buildingMap.nearCellsAndDoorsMap.find(
            el => el._1._1 == cellY && el._1._2 == cellX
          )

        // try to enter door
        if (doorDestinationOpt.nonEmpty) {

          val currentDoorCellState = newGrid.cells(doorDestinationOpt.get._2.y)(doorDestinationOpt.get._2.x)

          currentDoorCellState match {
            case PersonCell(smell, waitingPeople, isInCorridor) => {
              if (waitingPeople.size < 2) { // go to door cell with another person
                // wait in doors
                newGrid.cells(doorDestinationOpt.get._2.y)(doorDestinationOpt.get._2.x) =
                  cellToMove.copy(cellToMove.smell, cellToMove :: cellToMove.waitingPeople, false)
              }
              else {
                // don't move
                newGrid.cells(cellY)(cellX) = cellToMove
              }
            }
            case _ => {
              // go to door cell
              newGrid.cells(doorDestinationOpt.get._2.y)(doorDestinationOpt.get._2.x) = cellToMove
            }
          }
        }

        // in corridor or in room
        else {
          val destinations = calculatePossibleDestinations(cellToMove, cellY, cellX, grid)
          val destination = selectDestinationCell(destinations, newGrid, cellToMove.isInCorridor)

          destination match {
            case Opt((i, j, PersonAccessible(destination))) => {
              grid.cells(i)(j) match {

                  // try to teleport
                case TeleportationCell(id, _) => {
                  if (id != -1)
                    newGrid.cells(buildingMap.teleportationPairs(id).point2.y)(buildingMap.teleportationPairs(id).point2.x) = cellToMove
                }

                case ExitCell(_, _) => {
                  evacuatedCount += 1
                }

                case _ => {
                  newGrid.cells(i)(j) = destination.withPerson(List.empty, cellToMove.isInCorridor) //cell.waitingPeople.head.waitingPeople
                }
              }
              if (cellToMove == newGrid.cells(cellY)(cellX))
                newGrid.cells(cellY)(cellX) = EmptyCell(cellToMove.smell)
            }

            case Opt((i, j, inaccessibleDestination)) =>
              inaccessibleDestination match {
                case PersonCell(smell, waitingPeople, isInCorridor) => {

                  // go forward when in corridor - the person in front will move too
                  if (isInCorridor) newGrid.cells(i)(j) = cellToMove

                    // wrong possition if not in corridor
                  else throw new RuntimeException(s"Person selected inaccessible destination ($i,$j): $inaccessibleDestination")
                }
                case _ => {
                  throw new RuntimeException(s"Person selected inaccessible destination ($i,$j): $inaccessibleDestination")
                }
              }
            case Opt.Empty =>
              newGrid.cells(cellY)(cellX) = cellToMove
          }
        }
      }
    }

    def getCellWithUpdatedState(cellY: Int, cellX: Int, cell: PersonCell): PersonCell = {
      if (buildingMap.exitsToFloor1.exists(point => point.y == cellY && point.x == cellX))
        cell.copy(cell.smell, cell.waitingPeople, false)
      else
        cell.copy(cell.smell, cell.waitingPeople, cell.isInCorridor)
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

    def selectDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid, isInCorridor: Boolean): commons.Opt[(Int, Int, GridPart)] = {
      if (!isInCorridor) {
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
      else {
        possibleDestinations
          .map { case (i, j, current) => (i, j, current, newGrid.cells(i)(j)) }
          .collectFirstOpt {
            case (i, j, currentCell, _) => (i, j, currentCell)
          }
      }
    }

    if (iteration < boundaryIterationNo) propagateInitialSmell()
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

