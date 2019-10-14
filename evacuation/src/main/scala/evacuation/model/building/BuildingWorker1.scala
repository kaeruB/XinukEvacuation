package evacuation.model.building

class BuildingWorker1 {
  private val av_x = 199 // 200, 3 - 392
  private val bv_x = 201 // 202, 3 - 392
  private val cv_x = 239 // 240, 3 - 392
  private val dv_x = 241 // 242, 3 - 392

  private val v_start = 2 // 3
  private val v_end = 392 //393
  private val v_x: Array[Int] = Array(av_x, bv_x, cv_x, dv_x)

  private val ah_y = 2 // 3
  private val bh_y = 67 // 68
  private val ch_y = 132 // 133
  private val dh_y = 197 // 198
  private val eh_y = 262 // 263
  private val fh_y = 327 // 328
  private val gh_y = 392 // 393

  private val h_start = 202 // 203
  private val h_end = 238 //239
  private val h_y: Array[Int] = Array(ah_y, bh_y, ch_y, dh_y, eh_y, fh_y, gh_y)

  private val a_d_l = new Point(201, 34)
  private val a_d_r = new Point(239, 34)
  private val b_d_l = new Point(201, 99)
  private val b_d_r = new Point(239, 99)
  private val c_d_l = new Point(201, 164)
  private val c_d_r = new Point(239, 164)
  private val d_d_l = new Point(201, 229)
  private val d_d_r = new Point(239, 229)
  private val e_d_l = new Point(201, 294)
  private val e_d_r = new Point(239, 294)
  private val f_d_l = new Point(201, 359)
  private val f_d_r = new Point(239, 359)

  val wallsPoints: List[Point] = getWallsPoints
  val doorsPoints: Array[Point] = Array(
    a_d_l,
    a_d_r,
    b_d_l,
    b_d_r,
    c_d_l,
    c_d_r,
    d_d_l,
    d_d_r,
    e_d_l,
    e_d_r,
    f_d_l,
    f_d_r
  )

  private def getWallsPoints: List[Point] = {
    var wallsPointsList: List[Point] = List.empty

    // horizontal walls
    for (y <- v_start to v_end)
      for (xi <- v_x.indices)
        wallsPointsList = addPointToList(v_x(xi), y, wallsPointsList)

    // vertical walls
    for (x <- h_start to h_end)
      for (yi <- h_y.indices)
        wallsPointsList = addPointToList(x, h_y(yi), wallsPointsList)

    // inside horizontal walls
    for (x <- RoomCornersWorker1.leftMiddleCornerA.x + 1 until RoomCornersWorker1.rightMiddleCornerA.x) {
      wallsPointsList = addPointToList(x, RoomCornersWorker1.leftMiddleCornerA.y + 1, wallsPointsList)
      wallsPointsList = addPointToList(x, RoomCornersWorker1.rightMiddleCornerA.y - 1, wallsPointsList)

      wallsPointsList = addPointToList(x, RoomCornersWorker1.leftMiddleCornerB.y + 1, wallsPointsList)
      wallsPointsList = addPointToList(x, RoomCornersWorker1.rightMiddleCornerB.y - 1, wallsPointsList)

      wallsPointsList = addPointToList(x, RoomCornersWorker1.leftMiddleCornerC.y + 1, wallsPointsList)
      wallsPointsList = addPointToList(x, RoomCornersWorker1.rightMiddleCornerC.y - 1, wallsPointsList)

      wallsPointsList = addPointToList(x, RoomCornersWorker1.leftMiddleCornerD.y + 1, wallsPointsList)
      wallsPointsList = addPointToList(x, RoomCornersWorker1.rightMiddleCornerD.y - 1, wallsPointsList)

      wallsPointsList = addPointToList(x, RoomCornersWorker1.leftMiddleCornerE.y + 1, wallsPointsList)
      wallsPointsList = addPointToList(x, RoomCornersWorker1.rightMiddleCornerE.y - 1, wallsPointsList)

      wallsPointsList = addPointToList(x, RoomCornersWorker1.leftMiddleCornerF.y + 1, wallsPointsList)
      wallsPointsList = addPointToList(x, RoomCornersWorker1.rightMiddleCornerF.y - 1, wallsPointsList)
    }

    // inside vertical walls
    for (y <- RoomCornersWorker1.leftMiddleCornerA.y + 1 until RoomCornersWorker1.rightMiddleCornerA.y) {
      wallsPointsList = addPointToList(RoomCornersWorker1.leftMiddleCornerA.x + 1, y, wallsPointsList)
      wallsPointsList = addPointToList(RoomCornersWorker1.rightMiddleCornerA.x - 1, y, wallsPointsList)
    }
    for (y <- RoomCornersWorker1.leftMiddleCornerB.y + 1 until RoomCornersWorker1.rightMiddleCornerB.y) {
      wallsPointsList = addPointToList(RoomCornersWorker1.leftMiddleCornerB.x + 1, y, wallsPointsList)
      wallsPointsList = addPointToList(RoomCornersWorker1.rightMiddleCornerB.x - 1, y, wallsPointsList)
    }
    for (y <- RoomCornersWorker1.leftMiddleCornerC.y + 1 until RoomCornersWorker1.rightMiddleCornerC.y) {
      wallsPointsList = addPointToList(RoomCornersWorker1.leftMiddleCornerC.x + 1, y, wallsPointsList)
      wallsPointsList = addPointToList(RoomCornersWorker1.rightMiddleCornerC.x - 1, y, wallsPointsList)
    }
    for (y <- RoomCornersWorker1.leftMiddleCornerD.y + 1 until RoomCornersWorker1.rightMiddleCornerD.y) {
      wallsPointsList = addPointToList(RoomCornersWorker1.leftMiddleCornerD.x + 1, y, wallsPointsList)
      wallsPointsList = addPointToList(RoomCornersWorker1.rightMiddleCornerD.x - 1, y, wallsPointsList)
    }
    for (y <- RoomCornersWorker1.leftMiddleCornerE.y + 1 until RoomCornersWorker1.rightMiddleCornerE.y) {
      wallsPointsList = addPointToList(RoomCornersWorker1.leftMiddleCornerE.x + 1, y, wallsPointsList)
      wallsPointsList = addPointToList(RoomCornersWorker1.rightMiddleCornerE.x - 1, y, wallsPointsList)
    }
    for (y <- RoomCornersWorker1.leftMiddleCornerF.y + 1 until RoomCornersWorker1.rightMiddleCornerF.y) {
      wallsPointsList = addPointToList(RoomCornersWorker1.leftMiddleCornerF.x + 1, y, wallsPointsList)
      wallsPointsList = addPointToList(RoomCornersWorker1.rightMiddleCornerF.x - 1, y, wallsPointsList)
    }

    wallsPointsList
  }

  private def addPointToList(x: Int, y: Int, list: List[Point]): List[Point] = {
    new Point(x, y) :: list
  }
}
