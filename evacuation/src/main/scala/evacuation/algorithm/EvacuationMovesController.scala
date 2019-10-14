package evacuation.algorithm

import evacuation.config.EvacuationConfig
import evacuation.model.{EvacuationDirectionAccessible, EvacuationDirectionCell, PersonAccessible, PersonCell, WallCell}
import evacuation.simulation.EvacuationMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.{Cell, EmptyCell, Grid, GridPart, Obstacle, Signal, WorkerId}
import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import evacuation.model.building.{BuildingWorker1, BuildingWorker2, PeopleWorker1, PeopleWorker2, Point}
import evacuation.utils.DoorCell
import com.avsystem.commons.misc.Opt

import scala.collection.immutable.TreeSet
import scala.util.Random

// grid.cells(y)(x)

final class EvacuationMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: EvacuationConfig)  extends MovesController {

  private val random = new Random(System.nanoTime())

  override def initialGrid(id: WorkerId): (Grid, EvacuationMetrics) = {
    val grid = Grid.empty(bufferZone)
    val metrics = EvacuationMetrics(0)

    def test(): Unit = {
      if (id.value == 1) {
        grid.cells(2)(2) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
        grid.cells(6)(6) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
      }

      if (id.value == 3)
        grid.cells(config.gridSize - 1)(config.gridSize - 1) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection()
    }

    def topLeftWorker(): Unit = {
      def buildWalls(): Unit = {
        val buildingWorker1: BuildingWorker1 = new BuildingWorker1()
        val wallPoints = buildingWorker1.wallsPoints
        val doorPoints = buildingWorker1.doorsPoints

        for {
          i <- wallPoints.indices
          if !doorPoints.exists(p => p.x == wallPoints(i).x && p.y == wallPoints(i).y)
        }
          grid.cells(wallPoints(i).y)(wallPoints(i).x) = Obstacle
      }

      def placePeople(): Unit = {
        val peopleWorker1: PeopleWorker1 = new PeopleWorker1()

        for (j <- 0 until 6) {
          for (i <- peopleWorker1.peopleCoordinates(j).indices) {
            grid.cells(peopleWorker1.peopleCoordinates(j)(i).y)(peopleWorker1.peopleCoordinates(j)(i).x) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
          }
        }
      }

      buildWalls()
      placePeople()
    }

    def bottomLeftWorker(): Unit = {
      def buildWalls(): Unit = {
        val buildingWorker2: BuildingWorker2 = new BuildingWorker2()
        val wallPoints = buildingWorker2.wallsPoints
        val doorPoints = buildingWorker2.doorsPoints
        val escapePoints = buildingWorker2.escapePoints

        for {
          i <- wallPoints.indices
          if !doorPoints.exists(p => p.x == wallPoints(i).x && p.y == wallPoints(i).y)
        }
          grid.cells(wallPoints(i).y)(wallPoints(i).x) = Obstacle

        for (i <- escapePoints.indices)
          grid.cells(escapePoints(i).y)(escapePoints(i).x) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection()
      }

      def placePeople(): Unit = {
        val peopleWorker2: PeopleWorker2 = new PeopleWorker2()

        for (j <- 0 until 4) {
          for (i <- peopleWorker2.peopleCoordinates(j).indices) {
            grid.cells(peopleWorker2.peopleCoordinates(j)(i).y)(peopleWorker2.peopleCoordinates(j)(i).x) = PersonAccessible.unapply(EmptyCell.Instance).withPerson()
          }
        }
      }

      buildWalls()
      placePeople()
    }

    id.value match {
      case 1 => topLeftWorker()
      case 2 =>
      case 3 => bottomLeftWorker()
      case 4 =>
    }
//  bottomLeftWorker()
//  test()

    (grid, metrics)
  }

  override def makeMoves(iteration: Long, grid: Grid, id: WorkerId): (Grid, EvacuationMetrics) = {
    val newGrid = Grid.empty(bufferZone)
    var evacuatedThroughDoor0Count = 0L

    if (id.value == 1 || id.value == 3) {
      def getDynamicAndStaticCells(startPoint: Point, endPoint: Point): (IndexedSeq[(Int, Int, GridPart)], IndexedSeq[(Int, Int, GridPart)]) = {
        val (dynamicCells, staticCells) = (for {
//          y <- 0 until config.gridSize
//          x <- 0 until config.gridSize
          y <- startPoint.x until endPoint.x
          x <- startPoint.y until endPoint.y
        } yield (x, y, grid.cells(x)(y))).partition({
          case (_, _, Obstacle) => true
          case (_, _, EvacuationDirectionCell(_)) => true
          case (_, _, PersonCell(_)) => true
          case (_, _, _) => false
        })
        (dynamicCells, staticCells)
      }

      val (dynamicCells, staticCells): (IndexedSeq[(Int, Int, GridPart)], IndexedSeq[(Int, Int, GridPart)]) = {
        if (id.value == 1) {
          val startPoint = new Point(182, 0) // new Point(199, 0) // av_x
          val endPoint = new Point(272, config.gridSize) // new Point(241, config.gridSize) // dv_x
          getDynamicAndStaticCells(startPoint, endPoint)
        }
        else {
          val startPoint = new Point(182, 0)
          val endPoint = new Point(272, config.gridSize)
          getDynamicAndStaticCells(startPoint, endPoint)
        }
      }

      staticCells.foreach({
        case (x, y, cell) => copyCells(x, y, cell)
      })
      dynamicCells.foreach({
        case (x, y, cell: PersonCell) => movePersonCell(x, y, cell)
        case (x, y, cell: EvacuationDirectionCell) => copyEvacuationDirectionCell(x, y, cell)
        case (x, y, Obstacle) => copyWallCell(x, y, Obstacle)
        case (_, _, _) =>
      })

      def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
        newGrid.cells(x)(y) = cell
      }

      def movePersonCell(x: Int, y: Int, cell: PersonCell): Unit = {
        val destinations = calculatePossibleDestinations(cell, x, y, grid)
        val destination = selectDestinationCell(destinations, newGrid)

//        if (destination.nonEmpty) {
//          newGrid.cells(destination.get._1)(destination.get._2) = PersonCell(grid.cells(destination.get._1)(destination.get._2).smell)
//        }
//        else if (destination.isEmpty) {
//          newGrid.cells(x)(y) = cell
//        }

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

      def copyWallCell(x: Int, y: Int, cell: GridPart): Unit = {
        newGrid.cells(x)(y) = Obstacle
      }

      def copyEvacuationDirectionCell(x: Int, y: Int, cell: GridPart): Unit = {
        newGrid.cells(x)(y) = EvacuationDirectionAccessible.unapply(EmptyCell.Instance).withEvacuationDirection() //EvacuationDirectionCell.create(config.evacuationDirectionInitialSignal)
      }
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
}

