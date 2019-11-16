package evacuation.model

import evacuation.config.EvacuationConfig

import scala.util.Random

final class BuildingMap(implicit config: EvacuationConfig) {
  private val random = new Random(System.nanoTime())

  private val walls: Array[PointPair] = Array(
    // outside walls
    new PointPair(new Point(0, 0), new Point(0, 133)),
    new PointPair(new Point(0, 0), new Point(60, 0)),
    new PointPair(new Point(0, 134), new Point(60, 133)),
    new PointPair(new Point(60, 0), new Point(60, 133)),
    new PointPair(new Point(60, 0), new Point(120, 0)),
    new PointPair(new Point(120, 0), new Point(120, 133)),
    new PointPair(new Point(60, 134), new Point(120, 133)),

    // inside walls horizontal
    new PointPair(new Point(20, 50), new Point(20, 90)),
    new PointPair(new Point(22, 50), new Point(22, 90)),
    new PointPair(new Point(33, 50), new Point(33, 90)),
    new PointPair(new Point(35, 50), new Point(35, 90)),
    new PointPair(new Point(80, 50), new Point(80, 90)),
    new PointPair(new Point(82, 50), new Point(82, 90)),
    new PointPair(new Point(93, 50), new Point(93, 90)),
    new PointPair(new Point(95, 50), new Point(95, 90)),

    // inside walls vertical
    new PointPair(new Point(20, 50), new Point(35, 50)),
    new PointPair(new Point(80, 50), new Point(95, 50)),
    new PointPair(new Point(20, 90), new Point(35, 90)),
    new PointPair(new Point(80, 90), new Point(95, 90))

//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
//    new PointPair(new Point(), new Point()),
  )

  private object smellSources {
    val A = new Point(21, 51)
    val B = new Point(34, 89)
    val C = new Point(81, 51)
    val D = new Point(94, 89)
  }

  private object doors {
    val A = new Point(20, 55)
    val B = new Point(35, 85)
    val C = new Point(80, 55)
    val D = new Point(95, 85)
  }

  private object floors {
    val floor1: Array[PointPair] = Array(
      new PointPair(new Point(1,1), new Point(19, 132)),
      new PointPair(new Point(20, 1), new Point(35, 49)),
      new PointPair(new Point(20, 91), new Point(35, 132)),
      new PointPair(new Point(36, 1), new Point(59, 132))
    )
    val floor2: Array[PointPair] = Array(
      new PointPair(new Point(61,1), new Point(79, 132)),
      new PointPair(new Point(80, 1), new Point(95, 49)),
      new PointPair(new Point(80, 91), new Point(95, 132)),
      new PointPair(new Point(96, 1), new Point(119, 132))
    )
  }

  val wallsPoints: List[Point] = getWallsPoints
  val smellOrigins: Array[Point] = Array(smellSources.A, smellSources.B, smellSources.C, smellSources.D)
  val doorsPoints: Array[Point] = Array(doors.A, doors.B, doors.C, doors.D)
  val peoplePoints: List[Point] = getPeoplePoints
  val teleportationPairs: Array[PointPair] = Array(
        new PointPair(smellSources.A, new Point(smellSources.C.y, smellSources.B.x)),
        new PointPair(smellSources.B, new Point(smellSources.D.y, smellSources.A.x)),
        new PointPair(smellSources.C, new Point(config.gridSize - 2, 3)),
        new PointPair(smellSources.D, new Point(config.gridSize - 2, 5))
  )
  val doorsPointsWithAssociatedPointsOnCorridor: Array[(Point, Point, Point)] = Array(
    (doors.A, new Point(doors.A.y + 1, doors.A.x - 1), new Point(doors.A.y + 1, doors.A.x)),
    (doors.B, new Point(doors.B.y - 1, doors.B.x + 1), new Point(doors.B.y - 1, doors.B.x)),
    (doors.C, new Point(doors.C.y + 1, doors.C.x - 1), new Point(doors.C.y + 1, doors.C.x)),
    (doors.D, new Point(doors.D.y - 1, doors.D.x + 1), new Point(doors.D.y - 1, doors.D.x))
  )

  private def getWallsPoints: List[Point] = {
    var wallsPointsList: List[Point] = List.empty
    for (pointsPair <- walls.indices) {
      val newPointsList = getPointsList(walls(pointsPair).point1, walls(pointsPair).point2)
      wallsPointsList = wallsPointsList ++ newPointsList
    }
    wallsPointsList
  }

  private def getPointsList(start: Point, end: Point): List[Point] = {
    var result: List[Point] = List.empty

    if (start.x == end.x) {
      for (i <- start.y to end.y) {
        result = new Point(i, start.x) :: result
      }
    }
    else {
      for (i <- start.x to end.x) {
        result = new Point(start.y, i) :: result
      }
    }

    result
  }

  private def getPeoplePoints: List[Point] = {
    getPeopleOnFloor(floors.floor1, config.peopleNoFloor1) ++ getPeopleOnFloor(floors.floor2, config.peopleNoFloor2)
  }

  private def getPeopleOnFloor(floorParts: Array[PointPair], noOfPeople: Int): List[Point] = {
    var result: List[Point] = List.empty

    for (_ <- 0 to noOfPeople) {
      val randomFloorPart = floorParts(random.nextInt(4))

      val randomPointX = random.nextInt(randomFloorPart.point2.x - randomFloorPart.point1.x + 1) + randomFloorPart.point1.x
      val randomPointY = random.nextInt(randomFloorPart.point2.y - randomFloorPart.point1.y + 1) + randomFloorPart.point1.y

      result = new Point(randomPointY, randomPointX) :: result
    }

    result
  }
}