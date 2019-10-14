package evacuation.model.building

import scala.util.Random

object PeopleFabric {
  private val random = new Random(System.nanoTime())

  def getPersonCoordinates(personsNumber: Int, leftCorner: Point, leftMiddleCorner: Point, rightMiddleCorner: Point, rightCorner: Point): List[Point] = {
    var points: List[Point] = List.empty

    for (i <- 0 until personsNumber) {
      points = getPoint(leftCorner, leftMiddleCorner, rightMiddleCorner, rightCorner) :: points
    }

    points
  }

  def getPoint(leftCorner: Point, leftMiddleCorner: Point, rightMiddleCorner: Point, rightCorner: Point): Point = {
    random.nextInt(4) match {
      case 0 => {
        val x = random.nextInt(rightCorner.x - leftCorner.x) + leftCorner.x
        val y = random.nextInt(leftMiddleCorner.y - leftCorner.y) + leftCorner.y
        new Point(x, y)
      }
      case 1 => {
        val x = random.nextInt(leftMiddleCorner.x - leftCorner.x) + leftCorner.x
        val y = random.nextInt(rightMiddleCorner.y - leftMiddleCorner.y) + leftMiddleCorner.y
        new Point(x, y)
      }
      case 2 =>{
        val x = random.nextInt(rightCorner.x - rightMiddleCorner.x) + rightMiddleCorner.x
        val y = random.nextInt(rightMiddleCorner.y - leftMiddleCorner.y) + leftMiddleCorner.y
        new Point(x, y)
      }
      case 3 =>{
        val x = random.nextInt(rightCorner.x - leftCorner.x) + leftCorner.x
        val y = random.nextInt(rightCorner.y - rightMiddleCorner.y) + rightMiddleCorner.y
        new Point(x, y)
      }
    }
  }
}
