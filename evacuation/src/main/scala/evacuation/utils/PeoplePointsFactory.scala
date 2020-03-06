package evacuation.utils

import evacuation.config.EvacuationConfig
import evacuation.model.building.Point
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

final class PeopleInRooms(implicit config: EvacuationConfig){
  object peopleInRooms {
    val peopleICloakroom: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleICloakroomPath), config.peopleNoICloakroom)
    val peopleICorridor: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleICorridorPath  ) , config.peopleNoICorridor)
    val peopleII241: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleII241Path  )     , config.peopleNoII241)
    val peopleIICorridor : List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIICorridorPath  ), config.peopleNoIICorridor)
    val peopleIII323 : List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIII323Path  )    , config.peopleNoIII323)
    val peopleIII324 : List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIII324Path  )    , config.peopleNoIII324)
    val peopleIII327a: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIII327aPath  )   , config.peopleNoIII327a)
    val peopleIII327b: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIII327bPath  )   , config.peopleNoIII327b)
    val peopleIII327c: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIII327cPath  )   , config.peopleNoIII327c)
    val peopleIII327d: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIII327dPath  )   , config.peopleNoIII327d)
    val peopleIII327e: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIII327ePath  )   , config.peopleNoIII327e)
    val peopleIIICorridor: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIIICorridor  )   , config.peopleNoIIICorridor)
    val peopleIV426: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIV426Path  )     , config.peopleNoIV426)
    val peopleIV428: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIV428Path  )     , config.peopleNoIV428)
    val peopleIV429: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIV429Path  )     , config.peopleNoIV429)
    val peopleIV430: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIV430Path  )     , config.peopleNoIV430)
    val peopleIV431: List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIV431Path  )     , config.peopleNoIV431)
    val peopleIVCorridor : List[Point] = PeoplePointsFactory.getPeoplePointsList(ImgMapper.mapImgToPoints(pathsToRooms.peopleIVCorridorPath  ), config.peopleNoIVCorridor)
  }
}


object PeoplePointsFactory {
  def getPeoplePointsList(availablePointsToPlacePeople: List[Point], numberOfPeople: Int): List[Point] = {
    var peoplePointsOnGrid: List[Point] = List.empty
    val random = new Random(System.nanoTime())

    var availablePointsToPlacePeopleMutable: List[Point] = availablePointsToPlacePeople

    for (i <- 0 until numberOfPeople) {
      val randomIndex = random.nextInt(availablePointsToPlacePeopleMutable.length)
      peoplePointsOnGrid = availablePointsToPlacePeopleMutable(randomIndex) :: peoplePointsOnGrid
      availablePointsToPlacePeopleMutable = availablePointsToPlacePeopleMutable.take(randomIndex) ++ availablePointsToPlacePeopleMutable.take(randomIndex + 1)
    }

    peoplePointsOnGrid
  }

  def getAvailablePointsForPeople() = {

  }
}
