package evacuation.algorithm

import com.avsystem.commons.misc.Opt
import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import evacuation.config.EvacuationConfig
import evacuation.model.EvacuationDirectionSmellStrength.EvacuationDirectionSmellStrength
import evacuation.model.building.{Point, PointPair}
import evacuation.utils.PeopleInRooms
import evacuation.model.{EvacuationDirectionCell, EvacuationDirectionSmellStrength, ExitCell, PersonAccessible, PersonCell, TeleportationCell}
import evacuation.simulation.EvacuationMetrics
import evacuation.utils.ImgMapper
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, EmptyCell, Grid, GridPart, Obstacle, Signal}

import scala.collection.immutable.TreeSet

final class EvacuationMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: EvacuationConfig)  extends MovesController {

  val people: PeopleInRooms = new PeopleInRooms()

  val initialSmellPropagationMaxIteration = 14
  val initialSmellPropagationWithBottomDoorsClosedMaxIteration = 29

  val peopleICloakroomStartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 735
  val peopleICorridorStartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 456
  val peopleII241StartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 327
  val peopleIICorridorStartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 331
  val peopleIII323StartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 373
  val peopleIII324StartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 415
  val peopleIII327aStartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 509
  val peopleIII327bStartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 436
  val peopleIII327cStartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 391
  val peopleIII327dStartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 604
  val peopleIII327eStartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 462
  val peopleIIICorridorStartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 405
  val peopleIIICorridorFastStartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 27
  val peopleIV426StartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 382
  val peopleIV428StartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 364
  val peopleIV429StartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration +122
  val peopleIV430StartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 433
  val peopleIV431StartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration +115
  val peopleIVCorridorStartIteration: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 395
  val openBottomDoorsIterationNo: Int = initialSmellPropagationWithBottomDoorsClosedMaxIteration + 471

  var staticSmellFloor: Array[Array[SmellArray]] = Array.ofDim[SmellArray](config.gridSize, config.gridSize)
  var staticSmellFloorWithBottomDoorClosed: Array[Array[SmellArray]] = Array.ofDim[SmellArray](config.gridSize, config.gridSize)

  val exitsNo: Int = 4
  var evacuatedCounterByDoorId: Array[Int] = Array.ofDim[Int](exitsNo)

  val teleportationPairs: Array[PointPair] = ImgMapper.mapImgToTeleportationPairs("img/teleport_starts.png", "img/teleport_ends.png")

  var initialGridCopy: Grid = Grid.empty(bufferZone)
  val bottomDoorsPoints: List[Point] = ImgMapper.getBottomDoorsPoints("img/bottom_doors.png")
  val stairs: List[Point] = ImgMapper.mapStairsImgToPoints("img/stairs.png")

  override def initialGrid: (Grid, EvacuationMetrics) = {
    var grid = Grid.empty(bufferZone)
    val metrics = EvacuationMetrics(0, 0, 0, 0)

    grid = ImgMapper.mapImgToGrid("img/d17.png", grid)
    initialGridCopy = grid

    (grid, metrics)
  }



  override def makeMoves(iteration: Long, grid: Grid): (Grid, EvacuationMetrics) = {
    var newGrid = Grid.empty(bufferZone)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

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
      }  grid.cells(y)(x) match {
        case EvacuationDirectionCell(_, _, _) => {
          staticSmellFloor(y)(x) = grid.cells(y)(x).smell
        }
        case ExitCell(exitId, _) => {
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

    def resetSmellOnGrid(): Unit = {
      newGrid = initialGridCopy
    }

    def placeBottomDoors(): Unit = {
      bottomDoorsPoints.foreach(point => {
        newGrid.cells(point.y)(point.x) = Obstacle
      })
    }

    def removeBottomDoor(): Unit = {
      bottomDoorsPoints.foreach(point => {
        newGrid.cells(point.y)(point.x) = EmptyCell.Instance
      })
    }

    def createSmellSnapshotWithBottomDoorClosed(): Unit = {
      for {
        x <- 0 until config.gridSize
        y <- 0 until config.gridSize
      }  grid.cells(y)(x) match {
        case EvacuationDirectionCell(_, _, _) => {
          newGrid.cells(y)(x) = EmptyCell.Instance
          staticSmellFloorWithBottomDoorClosed(y)(x) = grid.cells(y)(x).smell
        }
        case ExitCell(exitId, _) => {
          newGrid.cells(y)(x) = ExitCell(exitId, Cell.emptySignal)
          staticSmellFloorWithBottomDoorClosed(y)(x) = grid.cells(y)(x).smell
        }
        case Obstacle => {
          newGrid.cells(y)(x) = grid.cells(y)(x)
        }
        case _ => {
          staticSmellFloorWithBottomDoorClosed(y)(x) = grid.cells(y)(x).smell
        }
      }

      placeTeleportationPairsOnGrid()
    }


    def placeTeleportationPairsOnGrid(): Unit = {
      for (i <- teleportationPairs.indices) {
        newGrid.cells(teleportationPairs(i).point1.y)(teleportationPairs(i).point1.x) = TeleportationCell(i, Cell.emptySignal)
      }
    }

    def placePeopleOnGridLinear(availablePointsList: (List[Point], Int)): (List[Point], Int) = {
      var availablePointsListCopy: (List[Point], Int) = availablePointsList

      if (availablePointsList._2 > 0) {

        var point = new Point(0, 0)
        var newPointFound = false

        while (!newPointFound) {
          point = people.getRandomPointForPerson(availablePointsList._1)
          newGrid.cells(point.y)(point.x) match {
            case PersonCell(_) =>
            case _ => newPointFound = true
          }
        }
        availablePointsListCopy = (availablePointsList._1, availablePointsList._2 - 1)

        newGrid.cells(point.y)(point.x) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      }

      availablePointsListCopy
    }

//    def placePeopleOnGridLinearInitialAlarm(): Unit = {
//      for (i <- buildingMap.peopleInitialAlarm.indices) {
//        if (buildingMap.peopleInitialAlarm(i)._2 > 0) {
//          var point = new Point(0, 0)
//          var newPointFound = false
//          while(!newPointFound) {
//            point = buildingMap.getPersonOnFloor(buildingMap.peopleInitialAlarm(i)._1)  // buildingMap.peopleGeneralAlarm(i).head
//            newGrid.cells(point.y)(point.x) match {
//              case PersonCell(_) =>
//              case _ => newPointFound = true
//            }
//          }
//          buildingMap.peopleInitialAlarm(i) = (buildingMap.peopleInitialAlarm(i)._1, buildingMap.peopleInitialAlarm(i)._2 - 1)
//
//          newGrid.cells(point.y)(point.x) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
//        }
//      }
//    }
//
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
      // version deterministic and random
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
      // moving version -- one more change to do! see below
      val destinations = calculatePossibleDestinations(cell, cellY, cellX, grid) // old grid
      val destination = selectDestinationCell(destinations, newGrid)

      // standing version -- one more change to do! see below
//      val destinations = calculatePossibleDestinations(cell, cellY, cellX, grid) // old grid
//      val destination = destinations.collectFirstOpt { case (i, j, cellInOldGrid) => (i, j, cellInOldGrid)} // selectDestinationCell(destinations, newGrid) // newGrid

      destination match {
        case Opt((i, j, PersonAccessible(destination))) => {
          grid.cells(i)(j) match {

            case TeleportationCell(id, _) => {
              if (id != -1) {
                val teleportationDestinationX = teleportationPairs(id).point2.x
                val teleportationDestinationY = teleportationPairs(id).point2.y

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
                case EmptyCell(_) => {
                    newGrid.cells(i)(j) = cell
                }
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
            case PersonCell(_) => {
              newGrid.cells(cellY)(cellX) = cell
            }
            case _ => {
              //standing  version -- one more change to do! see above
              // newGrid.cells(cellY)(cellX) = cell

              // moving version  -- one more change to do! see above
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
        .map { case (i, j) => {
            if (iteration < openBottomDoorsIterationNo) {
              cell.smell(i)(j) + staticSmellFloorWithBottomDoorClosed(cellY)(cellX)(i)(j)
            }
            else {
              cell.smell(i)(j) + staticSmellFloor(cellY)(cellX)(i)(j)
            }
          }
        }
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

    if (iteration < initialSmellPropagationMaxIteration) {
      propagateInitialSmell()
    }
    else if (iteration == initialSmellPropagationMaxIteration) {
      createSmellSnapshot()
      resetSmellOnGrid()
      placeBottomDoors()
    }
    else if (iteration < initialSmellPropagationWithBottomDoorsClosedMaxIteration) {
      propagateInitialSmell()
    }
    else if (iteration == initialSmellPropagationWithBottomDoorsClosedMaxIteration) {
      createSmellSnapshotWithBottomDoorClosed()
    }
    else {
      simulateEvacuation()
      if (iteration % 2 == 0) {
        if (iteration >= peopleICloakroomStartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleICloakroom = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleICloakroom)
        }
        if (iteration >= peopleICorridorStartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleICorridor = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleICorridor)
        }
        if (iteration >= peopleII241StartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleII241 = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleII241)
        }
        if (iteration >= peopleIICorridorStartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIICorridor = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIICorridor)
        }
        if (iteration >= peopleIII323StartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIII323 = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIII323)
        }
        if (iteration >= peopleIII324StartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIII324 = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIII324)
        }
        if (iteration >= peopleIII327aStartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIII327a = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIII327a)
        }
        if (iteration >= peopleIII327bStartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIII327b = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIII327b)
        }
        if (iteration >= peopleIII327cStartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIII327c = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIII327c)
        }
        if (iteration >= peopleIII327dStartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIII327d = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIII327d)
        }
        if (iteration >= peopleIII327eStartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIII327e = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIII327e)
        }
        if (iteration >= peopleIIICorridorStartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIIICorridor = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIIICorridor)
        }
        if (iteration >= peopleIIICorridorFastStartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIIICorridorFast = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIIICorridorFast)
        }
        if (iteration >= peopleIV426StartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIV426 = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIV426)
        }
        if (iteration >= peopleIV428StartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIV428 = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIV428)
        }
        if (iteration >= peopleIV429StartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIV429 = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIV429)
        }
        if (iteration >= peopleIV430StartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIV430 = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIV430)
        }
        if (iteration >= peopleIV431StartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIV431 = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIV431)
        }
        if (iteration >= peopleIVCorridorStartIteration) {
          people.notGroupedAvailablePointsWithPeopleNo.peopleIVCorridor = placePeopleOnGridLinear(people.notGroupedAvailablePointsWithPeopleNo.peopleIVCorridor)
        }
      }
      if (iteration == openBottomDoorsIterationNo) {
        removeBottomDoor()
      }

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
          evacuatedCounterByDoorId(3)
      val peopleCount = getNumberOfPeopleOnGrid()

      println(iteration + ": " + peopleCount + " " + evacuatedCount + " " + (peopleCount + evacuatedCount))
    }

    printPeopleEvacuatedAndOnGridNumber()

    val metrics = EvacuationMetrics(
      evacuatedCounterByDoorId(0),
      evacuatedCounterByDoorId(1),
      evacuatedCounterByDoorId(2),
      evacuatedCounterByDoorId(3)
    )

    (newGrid, metrics)
  }
}
