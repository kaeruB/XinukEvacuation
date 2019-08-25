package evacuation.algorithm

import evacuation.config.EvacuationConfig
import evacuation.model.{EvacuationDirectionAccessible, EvacuationDirectionCell, PersonAccessible, PersonCell, WallCell}
import evacuation.simulation.EvacuationMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.{EmptyCell, Grid, GridPart, Obstacle, Signal}
import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import evacuation.utils.DoorCell

import scala.collection.immutable.TreeSet
import scala.util.Random

final class EvacuationMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: EvacuationConfig)  extends MovesController {

  private val random = new Random(System.nanoTime())
  private val doorsCellsCoordinates: Array[List[DoorCell]] = new Array[List[DoorCell]](config.intermediateDoorNumber)
  private val escapeDoorCoordinates: Array[List[DoorCell]] = new Array[List[DoorCell]](config.escapeDoorNumber)

  override def initialGrid: (Grid, EvacuationMetrics) = {
    val grid = Grid.empty(bufferZone)
    val metrics = EvacuationMetrics(0)

    val horizontalGapBetweenOutsideInsideWalls = 20
    val horizontalInsideWallWidth = 28
    val verticalGapBetweenOutsideInsideWalls = 16
    val verticalInsideWallWidth = 81
    val verticalDoorUpperInsideWallDistance = 24
    val verticalDoorUpperInsideWallDistance2 = 57
    val doorWidth = 1

    def createHorizontalOutsideWalls(): Unit = {
      for (horizontal <- 0 until config.floorHorizontalWidth * config.floorNumber) {
        for (verticalFloor <- 0 until config.floorNumber)
          grid.cells(verticalFloor * config.floorVerticalWidth)(horizontal) = Obstacle // WallCell.create(config.wallInitialSignal) // WallAccessible.unapply(EmptyCell.Instance).withWall()
      }
    }

    def createVerticalOutsideWalls(): Unit = {
      for (vertical <- 0 until config.floorVerticalWidth) {
        for (horizontalFloor <- 0 until config.floorNumber)
          grid.cells(vertical)(horizontalFloor * config.floorHorizontalWidth) = Obstacle // WallCell.create(config.wallInitialSignal)
        for (horizontalFloor <- 1 until config.floorNumber + 1)
          grid.cells(vertical)(horizontalFloor * config.floorHorizontalWidth - 1) = Obstacle// WallCell.create(config.wallInitialSignal)
      }
    }

    def createOutsideWalls(): Unit = {
      createHorizontalOutsideWalls()
      createVerticalOutsideWalls()
    }

    def createInsideWalls(): Unit = {
      for (horizontal <- 0 until horizontalInsideWallWidth + 1) {
        for (horizontalFloor <- 0 until config.floorNumber) {
          grid.cells(verticalGapBetweenOutsideInsideWalls)(horizontalFloor * config.floorHorizontalWidth + horizontal + horizontalGapBetweenOutsideInsideWalls) = Obstacle// WallCell.create(config.wallInitialSignal)
          grid.cells(verticalGapBetweenOutsideInsideWalls + verticalInsideWallWidth)(horizontalFloor * config.floorHorizontalWidth + horizontal + horizontalGapBetweenOutsideInsideWalls) = Obstacle// WallCell.create(config.wallInitialSignal)
        }
      }

      for (vertical <- 0 until verticalInsideWallWidth) {
        for (horizontalFloor <- 0 until config.floorNumber) {
            grid.cells(vertical + verticalGapBetweenOutsideInsideWalls)(horizontalFloor * config.floorHorizontalWidth + horizontalGapBetweenOutsideInsideWalls) = Obstacle// WallCell.create(config.wallInitialSignal)
            grid.cells(vertical + verticalGapBetweenOutsideInsideWalls)(horizontalFloor * config.floorHorizontalWidth + horizontalGapBetweenOutsideInsideWalls + horizontalInsideWallWidth) = Obstacle// WallCell.create(config.wallInitialSignal)
        }
      }
    }

    def createCorridors(): Unit = {
      for (i <- 0 until config.escapeDoorNumber)
        escapeDoorCoordinates(i) = List.empty

      for (vertical <- 1 to 2) {
        for (horizontal <- 0 until config.floorNumber * config.floorHorizontalWidth) {
          grid.cells(config.floorVerticalWidth + 2 * vertical)(horizontal) = Obstacle// WallCell.create(config.wallInitialSignal)
        }
        grid.cells(config.floorVerticalWidth + 2 * vertical - 1)(config.floorNumber * config.floorHorizontalWidth - 1) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection()
        escapeDoorCoordinates(vertical - 1) = new DoorCell(config.floorVerticalWidth + 2 * vertical - 1, config.floorNumber * config.floorHorizontalWidth - 2) :: escapeDoorCoordinates(vertical - 1)
      }
    }

    def createEvacuationDirectionCells(): Unit = {

      for (i <- 0 until config.intermediateDoorNumber)
        doorsCellsCoordinates(i) = List.empty

      for (vertical <- verticalGapBetweenOutsideInsideWalls + verticalDoorUpperInsideWallDistance until verticalGapBetweenOutsideInsideWalls + verticalDoorUpperInsideWallDistance + doorWidth) {
        for (horizontalFloor <- 0 until config.floorNumber) {
          val horizontal = horizontalFloor * config.floorHorizontalWidth + horizontalGapBetweenOutsideInsideWalls
          grid.cells(vertical)(horizontal) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection()// EvacuationDirectionCell.create(config.evacuationDirectionInitialSignal) // EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection()
          doorsCellsCoordinates(horizontalFloor) = new DoorCell(vertical, horizontal) :: doorsCellsCoordinates(horizontalFloor)
        }
      }
      for (vertical <- verticalGapBetweenOutsideInsideWalls + verticalDoorUpperInsideWallDistance2 until verticalGapBetweenOutsideInsideWalls + verticalDoorUpperInsideWallDistance2 + doorWidth) {
        for (horizontalFloor <- 0 until config.floorNumber) {
          val horizontal = horizontalFloor * config.floorHorizontalWidth + horizontalGapBetweenOutsideInsideWalls + horizontalInsideWallWidth
          grid.cells(vertical)(horizontal) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection() // EvacuationDirectionCell.create(config.evacuationDirectionInitialSignal)
          doorsCellsCoordinates(horizontalFloor + 2) = new DoorCell(vertical, horizontal) :: doorsCellsCoordinates(horizontalFloor + 2)
        }
      }
    }

    def isEmptyCell(randomVertical: Int, randomHorizontal: Int): Boolean = {
      grid.cells(randomVertical)(randomHorizontal) match {
//        case WallCell(_) => false
        case Obstacle => false
        case PersonCell(_, _) => false
        case EvacuationDirectionCell(_) => false
        case _ => true
      }
    }

    def getPersonCoordinatesOnGrid(horizontalStartPoint: Int, verticalStartPoint: Int): (Int, Int) = {
      var randomHorizontal = random.nextInt(config.floorHorizontalWidth) + horizontalStartPoint
      var randomVertical = random.nextInt(config.floorVerticalWidth) + verticalStartPoint

      var placeFound = isEmptyCell(randomVertical, randomHorizontal)
      while (!placeFound) {
        randomHorizontal = random.nextInt(config.floorHorizontalWidth) + horizontalStartPoint
        randomVertical = random.nextInt(config.floorVerticalWidth) + verticalStartPoint

        placeFound = isEmptyCell(randomVertical, randomHorizontal)
      }

      (randomVertical, randomHorizontal)
    }

    def createPeople(): Unit = {


//      grid.cells(35)(12) = PersonAccessible.unapply(EmptyCell.Instance).withPerson(false)
       grid.cells(30)(77) = PersonAccessible.unapply(EmptyCell.Instance).withPerson(false)
       grid.cells(50)(77) = PersonAccessible.unapply(EmptyCell.Instance).withPerson(false)
//      grid.cells(39)(19) = PersonAccessible.unapply(EmptyCell.Instance).withPerson(false)
//      grid.cells(41)(19) = PersonAccessible.unapply(EmptyCell.Instance).withPerson(false)
      // floor 1 people
//      for (_ <- 0 until config.personsNumberFloor1) {
//        val (randomVertical, randomHorizontal) = getPersonCoordinatesOnGrid(0, 0)
//        grid.cells(randomVertical)(randomHorizontal) = PersonAccessible.unapply(EmptyCell.Instance).withPerson(false)
//      }
//
//      // floor 2 people
//      for (_ <- 0 until config.personsNumberFloor2) {
//        val (randomVertical, randomHorizontal) = getPersonCoordinatesOnGrid(config.floorHorizontalWidth, 0)
//        grid.cells(randomVertical)(randomHorizontal) = PersonAccessible.unapply(EmptyCell.Instance).withPerson(false)
//      }
    }

    createOutsideWalls()
    createInsideWalls()
    createCorridors()
    createEvacuationDirectionCells()
    createPeople()

    (grid, metrics)
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, EvacuationMetrics) = {
    val newGrid = Grid.empty(bufferZone)
    var evacuatedThroughDoor0Count = 0L

    resetDoorsCounters()

    val (dynamicCells, staticCells) = (for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } yield (x, y, grid.cells(x)(y))).partition({
      //case (_, _, WallCell(_)) => true
      case (_, _, Obstacle) => true
      case (_, _, EvacuationDirectionCell(_)) => true
      case (_, _, PersonCell(_, _)) => true
      case (_, _, _) => false
    })

    staticCells.foreach({
      case (x, y, cell) => copyCells(x, y, cell)
    })
    dynamicCells.foreach({
      case (x, y, cell: PersonCell) => movePersonCell(x, y, cell)
      case (x, y, cell: EvacuationDirectionCell) => copyEvacuationDirectionCell(x, y, cell)
//      case (x, y, cell: WallCell) => copyWallCell(x, y, cell)
      case (x, y, Obstacle) => copyWallCell(x, y, Obstacle)
      case (_, _, _) =>
    })

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def movePersonCell(x: Int, y: Int, cell: PersonCell): Unit = {
      val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)
      if (destination.nonEmpty) {
        if (!cell.reachedCorridor) {
          val doorId = reachedDoorId(destination.get._1, destination.get._2)
          doorId match {
            // TODO count how many people used the exit
            case -1 =>
              newGrid.cells(x)(y) = cell // don't move
            case 0 =>
              newGrid.cells(x)(y) = EmptyCell(cell.smell)
              newGrid.cells(config.floorVerticalWidth + 1)(1) = PersonCell(grid.cells(destination.get._1)(destination.get._2).smell, true)// PersonAccessible.unapply(EmptyCell.Instance).withPerson(true)
            case 1 =>
              newGrid.cells(x)(y) = EmptyCell(cell.smell)
              newGrid.cells(config.floorVerticalWidth + 1)(config.floorHorizontalWidth) =PersonCell(grid.cells(destination.get._1)(destination.get._2).smell, true) //PersonAccessible.unapply(EmptyCell.Instance).withPerson(true)
            case 2 =>
              newGrid.cells(x)(y) = EmptyCell(cell.smell)
              newGrid.cells(config.floorVerticalWidth + 3)(1) =PersonCell(grid.cells(destination.get._1)(destination.get._2).smell, true) // PersonAccessible.unapply(EmptyCell.Instance).withPerson(true)
            case 3 =>
              newGrid.cells(x)(y) = EmptyCell(cell.smell)
              newGrid.cells(config.floorVerticalWidth + 3)(config.floorHorizontalWidth) =PersonCell(grid.cells(destination.get._1)(destination.get._2).smell, true)// PersonAccessible.unapply(EmptyCell.Instance).withPerson(true)
            case _ =>
              newGrid.cells(destination.get._1)(destination.get._2) = cell
              newGrid.cells(x)(y) = EmptyCell(cell.smell)
          }
        }
        else {
          val escapeDoor = getEscapeDoor(destination.get._1, destination.get._2)
          escapeDoor match {
            case 0 =>
              evacuatedThroughDoor0Count += 1
              newGrid.cells(x)(y) = EmptyCell(cell.smell)
            case 1 =>
              newGrid.cells(x)(y) = EmptyCell(cell.smell)
            case _ =>
              newGrid.cells(destination.get._1)(destination.get._2) = PersonCell(grid.cells(destination.get._1)(destination.get._2).smell, true)// PersonAccessible.unapply(EmptyCell.Instance).withPerson(true)

              if (cell eq newGrid.cells(x)(y))                                                                                                  // if the (x,y) is not occupied by a different cell, check if the cell is the same as this one, if yes - remove it
                newGrid.cells(x)(y) = EmptyCell(cell.smell)
          }
        }
      }
      else if (destination.isEmpty) {
        newGrid.cells(x)(y) = cell
      }
    }

    def getEscapeDoor(x: Int, y: Int): Int = {
      for (doorId <- escapeDoorCoordinates.indices) {
        val doorCoordinatesIndex: Int= escapeDoorCoordinates(doorId).indexWhere(doorCell => doorCell.x == x && doorCell.y == y)
        if (doorCoordinatesIndex != -1) {
          escapeDoorCoordinates(doorId)(doorCoordinatesIndex).isDoorEmpty = false
          return doorId
        }
      }
      -1 // still not reached the doors
    }

    def isDoorCellOccupied(doorId: Int, x: Int, y: Int): Boolean = {
      doorsCellsCoordinates(doorId).exists(doorCell => doorCell.x == x && doorCell.y == y && doorCell.isDoorEmpty == false)
    }

    def willBeCorridorBehindDoorEmpty(x: Int, y: Int, doorId: Int): Boolean = {
      doorId match {
        case 0 => {
          newGrid.cells(config.floorVerticalWidth + 1)(1) match  {
            case PersonCell(_, _) => false
            case _ => true
          }
        }
        case 1 => {
          newGrid.cells(config.floorVerticalWidth + 1)(config.floorHorizontalWidth) match  {
            case PersonCell(_, _) => false
            case _ => true
          }
        }
        case 2 => {
          newGrid.cells(config.floorVerticalWidth + 2)(1) match {
            case PersonCell(_, _) => false
            case _ => true
          }
        }
        case 3 => {
          newGrid.cells(config.floorVerticalWidth + 2)(config.floorHorizontalWidth) match {
            case PersonCell(_, _) => false
            case _ => true
          }
        }
        case _ => true
      }
    }

    def reachedDoorId(x: Int, y: Int): Int = {
      for (doorId <- doorsCellsCoordinates.indices) {
        val doorCoordinatesIndex: Int= doorsCellsCoordinates(doorId).indexWhere(doorCell => doorCell.x == x && doorCell.y == y && doorCell.isDoorEmpty)
        val tmp = willBeCorridorBehindDoorEmpty(x, y, doorId)
        if (doorCoordinatesIndex != -1 && tmp) {
          doorsCellsCoordinates(doorId)(doorCoordinatesIndex).isDoorEmpty = false
          return doorId
        }
        else if (isDoorCellOccupied(doorId, x, y)) {
          return -1 // wait for doors
        }
      }
      -2 // still not reached the doors
    }

    def copyEvacuationDirectionCell(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection() //EvacuationDirectionCell.create(config.evacuationDirectionInitialSignal)
    }

    def copyWallCell(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = Obstacle// WallCell.create(config.wallInitialSignal)
    }

    def calculatePossibleDestinations(cell: PersonCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
      val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
      Grid.SubcellCoordinates
        .map { case (i, j) => cell.smell(i)(j) }
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
          case (i, j, currentCell@PersonAccessible(_), EvacuationDirectionAccessible(_)) =>
            (i, j, currentCell)
        }
    }

    val metrics = EvacuationMetrics(evacuatedThroughDoor0Count)
    (newGrid, metrics)
  }

  def resetDoorsCounters(): Unit = {
    for (doorId <- doorsCellsCoordinates.indices) {
      for (coordinatesListIndex <- doorsCellsCoordinates(doorId).indices) {
        doorsCellsCoordinates(doorId)(coordinatesListIndex).isDoorEmpty = true
      }
    }
  }
}

