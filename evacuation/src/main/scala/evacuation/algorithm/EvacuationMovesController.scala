package evacuation.algorithm

import com.avsystem.commons.misc.Opt
import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import evacuation.config.EvacuationConfig
import evacuation.model.{EvacuationDirectionAccessible, EvacuationDirectionCell, PersonAccessible, PersonCell, Point, TeleportationAccessible, TeleportationCell, TeleportationPair}
import evacuation.simulation.EvacuationMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, EmptyCell, Grid, GridPart, Obstacle, Signal}

import scala.collection.immutable.TreeSet
import scala.util.Random

//    grid.cells(y)(x).smell -> SmellArray  ((Signal, Signal, Signal), (Signal, Signal, Signal), (Signal, Signal, Signal))
//    grid.cells(y)(x).smell(y2 = 0, 1, 2)(x2 = 0, 1, 2) -> Signal, float number

final class EvacuationMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: EvacuationConfig)  extends MovesController {

  val boundaryIterationNo = 30
  var staticSmellFloor: Array[Array[SmellArray]] = Array.ofDim[SmellArray](config.gridSize, config.gridSize)
  private val random = new Random(System.nanoTime())

//  val smellSourceA = new Point(20, 55)
//  val smellSourceB = new Point(35, 85)
//  val smellSourceC = new Point(80, 55)
//  val smellSourceD = new Point(95, 85)

  val doorA = new Point(20, 55)
  val doorB = new Point(35, 85)
  val doorC = new Point(80, 55)
  val doorD = new Point(95, 85)

  val doorsPointsWithAssociatedPointsOnCorridor: Array[(Point, Point, Point)] = Array(
    (doorA, new Point(doorA.y + 1, doorA.x), new Point(doorA.y + 1, doorA.x + 1)),
    (doorB, new Point(doorB.y - 1, doorB.x), new Point(doorB.y - 1, doorB.x - 1)),
    (doorC, new Point(doorC.y + 1, doorC.x), new Point(doorC.y + 1, doorC.x + 1)),
    (doorD, new Point(doorD.y - 1, doorD.x), new Point(doorD.y - 1, doorD.x - 1))
  )

//  val smellSourceA = new Point(34, 51)
//  val smellSourceB = new Point(21, 89)
//  val smellSourceC = new Point(94, 51)
//  val smellSourceD = new Point(81, 89)

  val smellSourceA = new Point(21, 51)
  val smellSourceB = new Point(34, 89)
  val smellSourceC = new Point(81, 51)
  val smellSourceD = new Point(94, 89)

  val teleportationPairs: Array[TeleportationPair] = Array(
    new TeleportationPair(smellSourceA, new Point (smellSourceC.y, smellSourceB.x)),
    new TeleportationPair(smellSourceB, new Point (smellSourceD.y, smellSourceA.x)),
    new TeleportationPair(smellSourceC, new Point (config.gridSize - 2, 3)),
    new TeleportationPair(smellSourceD, new Point (config.gridSize - 2, 5))
  )

//  val : Array[TeleportationPair] = Array(
//    new TeleportationPair(smellSourceA, new Point(81, 89)),
//    new TeleportationPair(smellSourceB, new Point(94, 51))
//  )

  override def initialGrid: (Grid, EvacuationMetrics) = {
    val grid = Grid.empty(bufferZone)
    val metrics = EvacuationMetrics()

    def twoFloorsWithCorridors(): Unit = {
      for (x <- 0 until config.gridSize) {
        grid.cells(0)(x) = Obstacle
        grid.cells(60)(x) = Obstacle
        grid.cells(120)(x) = Obstacle
//        Corridors at the bottom
//        grid.cells(config.gridSize - 1)(x) = Obstacle
//        grid.cells(config.gridSize - 3)(x) = Obstacle
//        grid.cells(config.gridSize - 5)(x) = Obstacle
      }

      for (x <- 50 to 90) {
        grid.cells(20)(x) = Obstacle
        grid.cells(35)(x) = Obstacle

        grid.cells(80)(x) = Obstacle
        grid.cells(95)(x) = Obstacle
      }

      for (x <- 51 to 90) {// 52 to doorA.x + 1
        grid.cells(22)(x) = Obstacle
        grid.cells(82)(x) = Obstacle
      }
//      grid.cells(21)(doorA.x + 1) = Obstacle
//      grid.cells(81)(doorA.x + 1) = Obstacle

      for (x <- 50 to 89) { //  doorB.x - 1 to 88
        grid.cells(33)(x) = Obstacle
        grid.cells(93)(x) = Obstacle
      }
//      grid.cells(34)(doorB.x - 1) = Obstacle
//      grid.cells(94)(doorB.x - 1) = Obstacle


      for (y <- 20 until 35) {
        grid.cells(y)(50) = Obstacle
        grid.cells(y)(90) = Obstacle
      }

      for (y <- 80 until 95) {
        grid.cells(y)(50) = Obstacle
        grid.cells(y)(90) = Obstacle
      }

      grid.cells(smellSourceA.y)(smellSourceA.x) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection()
      grid.cells(smellSourceB.y)(smellSourceB.x) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection()
      grid.cells(smellSourceC.y)(smellSourceC.x) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection()
      grid.cells(smellSourceD.y)(smellSourceD.x) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection()

      grid.cells(config.gridSize - 1)(config.gridSize - 1) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection()

      grid.cells(doorA.y)(doorA.x) = EmptyCell(Cell.emptySignal)
      grid.cells(doorB.y)(doorB.x) = EmptyCell(Cell.emptySignal)
      grid.cells(doorC.y)(doorC.x) = EmptyCell(Cell.emptySignal)
      grid.cells(doorD.y)(doorD.x) = EmptyCell(Cell.emptySignal)
    }

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


    twoFloorsWithCorridors()
    // oneFloor()
    // bendedCorridor()

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
          val id = teleportationPairs.indexWhere(teleportPair => teleportPair.startPoint.x == x && teleportPair.startPoint.y == y)
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

//      staticSmellFloor(smellSourceA.y)(smellSourceA.x) = grid.cells(smellSourceA.y)(smellSourceA.x + 1).smell
//      staticSmellFloor(smellSourceB.y)(smellSourceB.x) = grid.cells(smellSourceB.y)(smellSourceB.x - 1).smell
//      staticSmellFloor(smellSourceC.y)(smellSourceC.x) = grid.cells(smellSourceC.y)(smellSourceC.x + 1).smell
//      staticSmellFloor(smellSourceD.y)(smellSourceD.x) = grid.cells(smellSourceD.y)(smellSourceD.x - 1).smell
    }

    def placePeopleOnGrid(): Unit = {
      newGrid.cells(doorA.y - 1)(doorA.x - 1) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      newGrid.cells(doorA.y - 1)(doorA.x + 1) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()

      newGrid.cells(doorA.y - 2)(doorA.x - 2) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      newGrid.cells(doorA.y - 2)(doorA.x + 2) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      newGrid.cells(doorA.y - 13)(doorA.x - 3) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      newGrid.cells(doorA.y - 14)(doorA.x + 4) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      newGrid.cells(doorA.y - 17)(doorA.x - 8) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      newGrid.cells(doorA.y - 16)(doorA.x + 7) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()

      newGrid.cells(70)(70) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      newGrid.cells(50)(50) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      newGrid.cells(100)(80) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()

      newGrid.cells(73)(73) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      newGrid.cells(76)(96) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      newGrid.cells(53)(53) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      newGrid.cells(98)(79) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()


      newGrid.cells(85)(85) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
//      newGrid.cells(76)(96) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
//      newGrid.cells(53)(53) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
//      newGrid.cells(98)(79) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
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
        doorsPointsWithAssociatedPointsOnCorridor.find(points => points._1.x == x && points._1.y == y)

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
                  newGrid.cells(teleportationPairs(id).endPoint.y)(teleportationPairs(id).endPoint.x) = cell.copy(cell.smell)
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

