package evacuation.utils

import evacuation.config.EvacuationConfig
import evacuation.model.building.{Point, PointPair}

import scala.util.Random

private object pathsToRooms {
  val peopleICloakroomPath                : String = "img/rooms/I_cloakroom.png"
  val peopleICorridorPath               : String = "img/rooms/I_corridor.png"
  val peopleII241Path               : String = "img/rooms/II_241.png"
  val peopleIICorridorPath                : String = "img/rooms/II_corridor.png"
  val peopleIII323Path                : String = "img/rooms/III_323.png"
  val peopleIII324Path                : String = "img/rooms/III_324.png"
  val peopleIII327aPath               : String = "img/rooms/III_327a.png"
  val peopleIII327bPath               : String = "img/rooms/III_327b.png"
  val peopleIII327cPath               : String = "img/rooms/III_327c.png"
  val peopleIII327dPath               : String = "img/rooms/III_327d.png"
  val peopleIII327ePath               : String = "img/rooms/III_327e.png"
  val peopleIIICorridor              : String = "img/rooms/III_Corridor.png"
  val peopleIV426Path               : String = "img/rooms/IV_426.png"
  val peopleIV428Path               : String = "img/rooms/IV_428.png"
  val peopleIV429Path               : String = "img/rooms/IV_429.png"
  val peopleIV430Path               : String = "img/rooms/IV_430.png"
  val peopleIV431Path               : String = "img/rooms/IV_431.png"
  val peopleIVCorridorPath                : String = "img/rooms/IV_corridor.png"
}

final class PeopleInRooms(implicit config: EvacuationConfig) {

  object availablePoint {
    val peopleICloakroom: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleICloakroomPath)
    val peopleICorridor: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleICorridorPath)
    val peopleII241: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleII241Path)
    val peopleIICorridor: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIICorridorPath)
    val peopleIII323: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIII323Path)
    val peopleIII324: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIII324Path)
    val peopleIII327a: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIII327aPath)
    val peopleIII327b: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIII327bPath)
    val peopleIII327c: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIII327cPath)
    val peopleIII327d: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIII327dPath)
    val peopleIII327e: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIII327ePath)
    val peopleIIICorridor: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIIICorridor)
    val peopleNoIIICorridorFast: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIIICorridor)
    val peopleIV426: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIV426Path)
    val peopleIV428: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIV428Path)
    val peopleIV429: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIV429Path)
    val peopleIV430: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIV430Path)
    val peopleIV431: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIV431Path)
    val peopleIVCorridor: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIVCorridorPath)
  }

  object notGroupedAvailablePointsWithPeopleNo {
    var peopleICloakroom:(List[Point], Int) = Tuple2(
      availablePoint.peopleICloakroom,
      config.peopleNoICloakroom
    )
    var peopleICorridor:(List[Point], Int) = Tuple2(
      availablePoint.peopleICorridor,
      config.peopleNoICorridor
    )
    var peopleII241:(List[Point], Int) = Tuple2(
      availablePoint.peopleII241,
      config.peopleNoII241
    )
    var peopleIICorridor:(List[Point], Int) = Tuple2(
      availablePoint.peopleIICorridor,
      config.peopleNoIICorridor
    )
    var peopleIII323:(List[Point], Int) = Tuple2(
      availablePoint.peopleIII323,
      config.peopleNoIII323
    )
    var peopleIII324:(List[Point], Int) = Tuple2(
      availablePoint.peopleIII324,
      config.peopleNoIII324
    )
    var peopleIII327a:(List[Point], Int) = Tuple2(
      availablePoint.peopleIII327a,
      config.peopleNoIII327a
    )
    var peopleIII327b:(List[Point], Int) = Tuple2(
      availablePoint.peopleIII327b,
      config.peopleNoIII327b
    )
    var peopleIII327c:(List[Point], Int) = Tuple2(
      availablePoint.peopleIII327c,
      config.peopleNoIII327c
    )
    var peopleIII327d:(List[Point], Int) = Tuple2(
      availablePoint.peopleIII327d,
      config.peopleNoIII327d
    )
    var peopleIII327e:(List[Point], Int) = Tuple2(
      availablePoint.peopleIII327e,
      config.peopleNoIII327e
    )
    var peopleIIICorridor:(List[Point], Int) = Tuple2(
      availablePoint.peopleIIICorridor,
      config.peopleNoIIICorridor
    )
    var peopleIIICorridorFast:(List[Point], Int) = Tuple2(
      availablePoint.peopleNoIIICorridorFast,
      config.peopleNoIIICorridorFast
    )
    var peopleIV426:(List[Point], Int) = Tuple2(
      availablePoint.peopleIV426,
      config.peopleNoIV426
    )
    var peopleIV428:(List[Point], Int) = Tuple2(
      availablePoint.peopleIV428,
      config.peopleNoIV428
    )
    var peopleIV429:(List[Point], Int) = Tuple2(
      availablePoint.peopleIV429,
      config.peopleNoIV429
    )
    var peopleIV430:(List[Point], Int) = Tuple2(
      availablePoint.peopleIV430,
      config.peopleNoIV430
    )
    var peopleIV431:(List[Point], Int) = Tuple2(
      availablePoint.peopleIV431,
      config.peopleNoIV431
    )
    var peopleIVCorridor:(List[Point], Int) = Tuple2(
      availablePoint.peopleIVCorridor,
      config.peopleNoIVCorridor
    )
  }

  def getRandomPointForPerson(availablePoints: List[Point]): Point = {
    val random = new Random(System.nanoTime())
    val randomIndex = random.nextInt(availablePoints.length)
    availablePoints(randomIndex)
  }

}
