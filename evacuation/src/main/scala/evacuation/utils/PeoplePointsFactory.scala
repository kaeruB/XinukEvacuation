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
    val peopleIV426: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIV426Path)
    val peopleIV428: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIV428Path)
    val peopleIV429: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIV429Path)
    val peopleIV430: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIV430Path)
    val peopleIV431: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIV431Path)
    val peopleIVCorridor: List[Point] = ImgMapper.mapImgToPoints(pathsToRooms.peopleIVCorridorPath)
  }

  object groupedAvailablePoints {
    val level4: List[Point] =
      availablePoint.peopleIV426 :::
        availablePoint.peopleIV428 :::
        availablePoint.peopleIV429 :::
        availablePoint.peopleIV430 :::
        availablePoint.peopleIV431 :::
        availablePoint.peopleIVCorridor

    val level2: List[Point] =
      availablePoint.peopleII241 :::
        availablePoint.peopleIICorridor

    val level3Main: List[Point] =
      availablePoint.peopleIII327a :::
        availablePoint.peopleIII327b :::
        availablePoint.peopleIII327c :::
        availablePoint.peopleIII327d :::
        availablePoint.peopleIII327e :::
        availablePoint.peopleIIICorridor

    val level3Side: List[Point] = availablePoint.peopleIII323 ::: availablePoint.peopleIII324
    val level1: List[Point] = availablePoint.peopleICorridor
    val cloakroom: List[Point] = availablePoint.peopleICloakroom
  }

  object groupedAvailablePointsWithPeopleNo {
    var level4: (List[Point], Int) = Tuple2(
      groupedAvailablePoints.level4,
      config.peopleNoIV426 +
        config.peopleNoIV428 +
        config.peopleNoIV429 +
        config.peopleNoIV430 +
        config.peopleNoIV431 +
        config.peopleNoIVCorridor
    )

    var level3Main: (List[Point], Int) = Tuple2(
      groupedAvailablePoints.level3Main,
        config.peopleNoIII327a +
        config.peopleNoIII327b +
        config.peopleNoIII327c +
        config.peopleNoIII327d +
        config.peopleNoIII327e +
        config.peopleNoIIICorridor
    )

    var level3Side: (List[Point], Int) = Tuple2(
      groupedAvailablePoints.level3Side,
      config.peopleNoIII323 +
        config.peopleNoIII324
    )

    var level2: (List[Point], Int) = Tuple2(
      groupedAvailablePoints.level2,
      config.peopleNoII241 +
        config.peopleNoIICorridor
    )

    var level1: (List[Point], Int) = Tuple2(
      groupedAvailablePoints.level1,
      config.peopleNoICorridor
    )

    var cloakroom: (List[Point], Int) = Tuple2(
      groupedAvailablePoints.cloakroom,
      config.peopleNoICloakroom
    )
  }

  def getRandomPointForPerson(availablePoints: List[Point]): Point = {
    val random = new Random(System.nanoTime())
    val randomIndex = random.nextInt(availablePoints.length)
    availablePoints(randomIndex)
  }

}
//  object peopleInRooms {
//    val peopleICloakroom: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleICloakroomPath),    config.peopleNoICloakroom)
//    val peopleICorridor: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleICorridorPath  ) ,   config.peopleNoICorridor)
//    val peopleII241: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleII241Path  )     ,       config.peopleNoII241)
//    val peopleIICorridor : List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIICorridorPath  ), config.peopleNoIICorridor)
//    val peopleIII323 : List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIII323Path  )    ,     config.peopleNoIII323)
//    val peopleIII324 : List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIII324Path  )    ,     config.peopleNoIII324)
//    val peopleIII327a: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIII327aPath  )   ,     config.peopleNoIII327a)
//    val peopleIII327b: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIII327bPath  )   ,     config.peopleNoIII327b)
//    val peopleIII327c: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIII327cPath  )   ,     config.peopleNoIII327c)
//    val peopleIII327d: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIII327dPath  )   ,     config.peopleNoIII327d)
//    val peopleIII327e: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIII327ePath  )   ,     config.peopleNoIII327e)
//    val peopleIIICorridor: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIIICorridor  )   , config.peopleNoIIICorridor)
//    val peopleIV426: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIV426Path  )     ,       config.peopleNoIV426)
//    val peopleIV428: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIV428Path  )     ,       config.peopleNoIV428)
//    val peopleIV429: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIV429Path  )     ,       config.peopleNoIV429)
//    val peopleIV430: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIV430Path  )     ,       config.peopleNoIV430)
//    val peopleIV431: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIV431Path  )     ,       config.peopleNoIV431)
//    val peopleIVCorridor : List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIVCorridorPath  ), config.peopleNoIVCorridor)
//  }
//}
//
//
//object PeoplePointsFactory {
//  def getPeoplePointsList(availablePointsToPlacePeople: List[Point], numberOfPeople: Int): List[Point] = {
//    var peoplePointsOnGrid: List[Point] = List.empty
//    val random = new Random(System.nanoTime())
//
//    var availablePointsToPlacePeopleMutable: List[Point] = availablePointsToPlacePeople
//
//    for (i <- 0 until numberOfPeople) {
//      val randomIndex = random.nextInt(availablePointsToPlacePeopleMutable.length)
//      peoplePointsOnGrid = availablePointsToPlacePeopleMutable(randomIndex) :: peoplePointsOnGrid
//      availablePointsToPlacePeopleMutable = availablePointsToPlacePeopleMutable.take(randomIndex) ++ availablePointsToPlacePeopleMutable.take(randomIndex + 1)
//    }
//
//    peoplePointsOnGrid
//  }
//}
