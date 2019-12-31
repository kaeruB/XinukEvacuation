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
  val startSecondPhaseOfEvacuationIterationNo = 220
  var staticSmellFloor: Array[Array[SmellArray]] = Array.ofDim[SmellArray](config.gridSize, config.gridSize)
  val buildingMap: BuildingMap = new BuildingMap()

  val exitsNo: Int = 6
  var evacuatedCounterByDoorId: Array[Int] = Array.ofDim[Int](exitsNo)

  override def initialGrid: (Grid, EvacuationMetrics) = {
    val grid = Grid.empty(bufferZone)
    val metrics = EvacuationMetrics(0, 0, 0, 0, 0, 0)

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
            val foundExit = buildingMap.exits.find(exitEl => exitEl._1.x == x && exitEl._1.y == y)
            var id = 0;
            if (foundExit.nonEmpty)
              id = foundExit.get._3
            newGrid.cells(y)(x) = ExitCell(id, Cell.emptySignal)
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
      for (point <- buildingMap.peoplePointsOnFloor567) {
        newGrid.cells(point.y)(point.x) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      }
    }

    def placePeopleOnGridSecondPhase(): Unit = {
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

    def getShuffledIndexes(): List[Int] = {
      scala.util.Random.shuffle(List.range(0, config.gridSize))
    }

    def getDynamicAndStaticCellsShuffled(): (List[(Int, Int, GridPart)], List[(Int, Int, GridPart)]) = {
      val xShuffled = getShuffledIndexes()
      val yShuffled = getShuffledIndexes()

      val (dynamicCells, staticCells) = (for {
        y <- yShuffled
        x <- xShuffled
      } yield (x, y, grid.cells(x)(y))).partition({
        case (_, _, PersonCell(_)) => true
        case (_, _, _) => false
      })

      (dynamicCells, staticCells)
    }

    def getDynamicAndStaticCells(): (List[(Int, Int, GridPart)], List[(Int, Int, GridPart)]) = {
      val indexes: List[Int] = List.range(0, config.gridSize)

      val (dynamicCells, staticCells) = (for {
        y <- indexes
        x <- indexes
      } yield (x, y, grid.cells(x)(y))).partition({
        case (_, _, PersonCell(_)) => true
        case (_, _, _) => false
      })

      (dynamicCells, staticCells)
    }

    def simulateEvacuation(): Unit = {
      val (dynamicCells, staticCells) = getDynamicAndStaticCellsShuffled() // getDynamicAndStaticCells() getDynamicAndStaticCellsShuffled()

      staticCells.foreach({
        case (x, y, Obstacle) => newGrid.cells(x)(y) = Obstacle
        case (x, y, cell) => copyCells(x, y, cell)
      })

      dynamicCells.foreach({
        case (x, y, cell: PersonCell) => movePersonCell(x, y, cell)
        case (_, _, _) =>
      })
    }

    def movePersonCell(cellY: Int, cellX: Int, cell: PersonCell): Unit = {
      val destinations = calculatePossibleDestinations(cell, cellY, cellX, grid) // old grid
      val destination = selectDestinationCell(destinations, newGrid) // newGrid

      destination match {
        case Opt((i, j, PersonAccessible(destination))) => {
          grid.cells(i)(j) match {

            case TeleportationCell(id, _) => {
              if (id != -1) {
                val teleportationDestinationX = buildingMap.teleportationPairs(id).point2.x
                val teleportationDestinationY = buildingMap.teleportationPairs(id).point2.y

                  newGrid.cells(teleportationDestinationY)(teleportationDestinationX) match {
                  case EmptyCell(_) => {
                    grid.cells(teleportationDestinationY)(teleportationDestinationX) match {
                      case EmptyCell(_) => {
                        newGrid.cells(teleportationDestinationY)(teleportationDestinationX) = cell
                      }
                      case _ => newGrid.cells(cellY)(cellX) = cell
                    }
                  }
                  case _ => newGrid.cells(cellY)(cellX) = cell
                }
              }
              else println("Error - teleportation destination not found")
            }

            case ExitCell(id, _) => {
              evacuatedCounterByDoorId(id) += 1
            }

            case PersonCell(_) => {
              // stay in place
              newGrid.cells(cellY)(cellX) = cell
            }

            case EmptyCell(_) => {
              // check if newGrid is empty, if yes - go, if not - stay
              newGrid.cells(i)(j) match {
                case EmptyCell(_) => newGrid.cells(i)(j) = cell
                case _ => newGrid.cells(cellY)(cellX) = cell
              }
            }

            case foundType => {
              println("Error - unexpected type in accessible by person cells " + foundType)
            }
          }
        }

        case Opt((i, j, inaccessibleDestination)) =>
          inaccessibleDestination match {
            case _ => {
              throw new RuntimeException(s"Person selected inaccessible destination ($i,$j): $inaccessibleDestination")
            }
          }
        case Opt.Empty => {
          newGrid.cells(cellY)(cellX) match {
            case EmptyCell(_) => newGrid.cells(cellY)(cellX) = cell
            case _ => println("Error - overwriting non empty cell")
          }
        }
      }
    }

    def calculatePossibleDestinations(cell: PersonCell, cellY: Int, cellX: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
      val neighbourCellCoordinates = Grid.neighbourCellCoordinates(cellY, cellX)

      Grid.SubcellCoordinates
        .map { case (i, j) => cell.smell(i)(j) + staticSmellFloor(cellY)(cellX)(i)(j) }
        .zipWithIndex
        .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
        .iterator
        .map { case (_, idx) =>
          val (i, j) = neighbourCellCoordinates(idx)
          (i, j, grid.cells(i)(j))
        }
    }

    def selectDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): commons.Opt[(Int, Int, GridPart)] = {
      possibleDestinations
        .map { case (i, j, current) => (i, j, current, newGrid.cells(i)(j)) }
        .collectFirstOpt {
          case (i, j, currentCell@PersonAccessible(_), PersonAccessible(_)) =>
            (i, j, currentCell)
        }
    }

    def createExitCells(): Unit = {
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
    else {
      simulateEvacuation()
      if (iteration == startSecondPhaseOfEvacuationIterationNo)
        placePeopleOnGridSecondPhase()
    }

    def printPeopleEvacuatedAndOnGridNumber(): Unit = {

      def getNumberOfPeopleOnGrid(): Int = {
        val (personCells, _) = (for {
          y <- 0 until config.gridSize
          x <- 0 until config.gridSize
        } yield (x, y, newGrid.cells(x)(y))).partition({
          case (_, _, PersonCell(_)) =>
            true
          case (_, _, _) => false
        })

        personCells.length
      }

      val evacuatedCount =
        evacuatedCounterByDoorId(0) +
          evacuatedCounterByDoorId(1) +
          evacuatedCounterByDoorId(2) +
          evacuatedCounterByDoorId(3) +
          evacuatedCounterByDoorId(4) +
          evacuatedCounterByDoorId(5)
      val peopleCount = getNumberOfPeopleOnGrid()

      println(iteration + ": " + peopleCount + " " + evacuatedCount + " " + (peopleCount + evacuatedCount))
    }

    printPeopleEvacuatedAndOnGridNumber()


    val metrics = EvacuationMetrics(
      evacuatedCounterByDoorId(0),
      evacuatedCounterByDoorId(1),
      evacuatedCounterByDoorId(2),
      evacuatedCounterByDoorId(3),
      evacuatedCounterByDoorId(4),
      evacuatedCounterByDoorId(5)
    )

    (newGrid, metrics)
  }
}

