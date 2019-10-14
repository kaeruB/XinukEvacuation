package evacuation.model.building

import evacuation.config.EvacuationConfig

class PeopleWorker2(implicit config: EvacuationConfig) {

  val peopleCoordinates: Array[List[Point]] = Array(
    PeopleFabric.getPersonCoordinates(config.personsNumberFloor12, RoomCornersWorker1.leftCornerA, RoomCornersWorker1.leftMiddleCornerA, RoomCornersWorker1.rightMiddleCornerA, RoomCornersWorker1.rightCornerA),
    PeopleFabric.getPersonCoordinates(config.personsNumberFloor11, RoomCornersWorker1.leftCornerB, RoomCornersWorker1.leftMiddleCornerB, RoomCornersWorker1.rightMiddleCornerB, RoomCornersWorker1.rightCornerB),
    PeopleFabric.getPersonCoordinates(config.personsNumberFloor10, RoomCornersWorker1.leftCornerC, RoomCornersWorker1.leftMiddleCornerC, RoomCornersWorker1.rightMiddleCornerC, RoomCornersWorker1.rightCornerC),
    PeopleFabric.getPersonCoordinates(config.personsNumberFloor9, RoomCornersWorker1.leftCornerD, RoomCornersWorker1.leftMiddleCornerD, RoomCornersWorker1.rightMiddleCornerD, RoomCornersWorker1.rightCornerD)
  )




}
