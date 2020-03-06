package evacuation.utils
import evacuation.config.EvacuationConfig
import javax.imageio.ImageIO
import pl.edu.agh.xinuk.model.{Cell, Grid, Obstacle}
import java.awt.Color
import java.awt.image.BufferedImage

import evacuation.model.building.{Point, PointPair}
import evacuation.model.{EvacuationDirectionCell, EvacuationDirectionSmellStrength, ExitCell}

object ImgMapper {
  val (white, black, blue) = (new Color(255, 255, 255), new Color(0, 0, 0), new Color(0, 0, 255))
  val (exit1, exit2, exit3, exit4) = (new Color(255, 0, 0), new Color(0, 0, 255), new Color(0, 255, 0), new Color(0, 255, 255))
  val smellSources = new Color(255, 0, 255)
  val (
    teleportation1a,
    teleportation1b,
    teleportation2a,
    teleportation2b,
    teleportation3a,
    teleportation3b,
    teleportation4a,
    teleportation4b,
    teleportation5a,
    teleportation5b
    ) = (
    new Color(0, 0, 0),
    new Color(0, 0, 255),
    new Color(0, 125, 0),
    new Color(0, 125, 255),
    new Color(0, 255, 0),
    new Color(0, 255, 255),
    new Color(125, 0, 0),
    new Color(125, 0, 255),
    new Color(125, 125, 0),
    new Color(125, 125, 255),
  )

//  def getBuildingMapBasedOnImages(): BuildingMap = {
//    val buildingMap: BuildingMap = new BuildingMap(
//
//      wallsPoints: List[Point],
//      smellOrigins: Array[Point],
//      exits: Array[(Point, EvacuationDirectionSmellStrength, Int)],
//      people: Array[PointPair],
//      teleportationPairs: Array[PointPair]
//    )
//    // wall points - ok
//    // people points - ok
//    // teleportation pairs - ok
//    // smell origins - ok
//    // exit points - ok
//  }

  def loadImg(name: String)(implicit config: EvacuationConfig): BufferedImage = {
    println(name)
    val img = ImageIO.read(getClass.getClassLoader.getResource(name))
    val (w, h) = (img.getWidth, img.getHeight)

    if (w != h || w != config.gridSize) {
      println("Wrong image size")
    }

    img
  }

  def mapImgToGrid(name: String, grid: Grid)(implicit config: EvacuationConfig): Grid = {
    val img = loadImg(name)


    for {
      x <- grid.cells.indices
      y <- grid.cells.indices
    } {
      new Color(img.getRGB(x, y)) match {
        case color if color == white =>
        case color if color == black => grid.cells(y)(x) = Obstacle
        case color if color == exit1 => grid.cells(y)(x) = ExitCell(1, Cell.emptySignal)
        case color if color == exit2 => grid.cells(y)(x) = ExitCell(2, Cell.emptySignal)
        case color if color == exit3 => grid.cells(y)(x) = ExitCell(3, Cell.emptySignal)
        case color if color == exit4 => grid.cells(y)(x) = ExitCell(4, Cell.emptySignal)
          // TODO EvacuationDirectionCell -> ustaw exit na true jesli jest wyjsciem!
        case color if color == smellSources => grid.cells(y)(x) = EvacuationDirectionCell.create(config.evacuationDirectionInitialSignal, false, EvacuationDirectionSmellStrength.Strong)
        case _ => println("blad")
      }
    }
    grid
  }

  def mapImgToTeleportationPairs(teleportationStartsImg: String, teleportationDestinationsImg: String)(implicit config: EvacuationConfig): Array[PointPair] = {
    val teleportationStartsImgParsed = loadImg(teleportationStartsImg)
    val teleportationDestinationsImgParsed = loadImg(teleportationDestinationsImg)

    val teleportationPairsNumber = 10

    val teleportationPairs: Array[PointPair] = Array.ofDim[PointPair](teleportationPairsNumber)

    for (i <- 0 until teleportationPairsNumber) teleportationPairs(i) = new PointPair(null, null)

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } {
      // map teleportation sources
      new Color(teleportationStartsImgParsed.getRGB(x, y)) match {
        case color if color == white =>
        case color if color == teleportation1a => teleportationPairs(0) = new PointPair(new Point(y, x), teleportationPairs(0).point2)
        case color if color == teleportation1b => teleportationPairs(1) = new PointPair(new Point(y, x), teleportationPairs(1).point2)
        case color if color == teleportation2a => teleportationPairs(2) = new PointPair(new Point(y, x), teleportationPairs(2).point2)
        case color if color == teleportation2b => teleportationPairs(3) = new PointPair(new Point(y, x), teleportationPairs(3).point2)
        case color if color == teleportation3a => teleportationPairs(4) = new PointPair(new Point(y, x), teleportationPairs(4).point2)
        case color if color == teleportation3b => teleportationPairs(5) = new PointPair(new Point(y, x), teleportationPairs(5).point2)
        case color if color == teleportation4a => teleportationPairs(6) = new PointPair(new Point(y, x), teleportationPairs(6).point2)
        case color if color == teleportation4b => teleportationPairs(7) = new PointPair(new Point(y, x), teleportationPairs(7).point2)
        case color if color == teleportation5a => teleportationPairs(8) = new PointPair(new Point(y, x), teleportationPairs(8).point2)
        case color if color == teleportation5b => teleportationPairs(9) = new PointPair(new Point(y, x), teleportationPairs(9).point2)
        case _ => println("blad")
      }
      // map teleportation destinations
      new Color(teleportationDestinationsImgParsed.getRGB(x, y)) match {
        case color if color == white =>
        case color if color == teleportation1a => teleportationPairs(0) = new PointPair(teleportationPairs(0).point1, new Point(y, x))
        case color if color == teleportation1b => teleportationPairs(1) = new PointPair(teleportationPairs(1).point1, new Point(y, x))
        case color if color == teleportation2a => teleportationPairs(2) = new PointPair(teleportationPairs(2).point1, new Point(y, x))
        case color if color == teleportation2b => teleportationPairs(3) = new PointPair(teleportationPairs(3).point1, new Point(y, x))
        case color if color == teleportation3a => teleportationPairs(4) = new PointPair(teleportationPairs(4).point1, new Point(y, x))
        case color if color == teleportation3b => teleportationPairs(5) = new PointPair(teleportationPairs(5).point1, new Point(y, x))
        case color if color == teleportation4a => teleportationPairs(6) = new PointPair(teleportationPairs(6).point1, new Point(y, x))
        case color if color == teleportation4b => teleportationPairs(7) = new PointPair(teleportationPairs(7).point1, new Point(y, x))
        case color if color == teleportation5a => teleportationPairs(8) = new PointPair(teleportationPairs(8).point1, new Point(y, x))
        case color if color == teleportation5b => teleportationPairs(9) = new PointPair(teleportationPairs(9).point1, new Point(y, x))
        case _ => println("blad")
      }
    }

    teleportationPairs
  }

  def mapImgToPoints(name: String)(implicit config: EvacuationConfig): List[Point] = {
    val img = loadImg(name)
    var pointsList: List[Point] = List.empty

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } {

      new Color(img.getRGB(x, y)) match {
        case color if color == white =>
        case color if color == black => {
          pointsList = new Point(y, x) :: pointsList
        }
        case _ => println("blad")
      }
    }

    pointsList
  }

  def colorDistance(c: Color, c2: Color): Double = {
    Math.sqrt((c.getRed - c2.getRed) * (c.getRed - c2.getRed)
      + (c.getGreen - c2.getGreen) * (c.getGreen - c2.getGreen)
      + (c.getBlue - c2.getBlue) * (c.getBlue - c2.getBlue))
  }

  def similarTo(color: Color, similarToColor: Color) = {
    val distance = colorDistance(color, similarToColor)
    distance < 200
  }
}
