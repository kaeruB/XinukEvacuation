package evacuation.model.building

class BuildingWorker2 {
  private val av_x = 199 // 200
  private val bv_x = 201 // 202
  private val cv_x = 239 // 240
  private val dv_x = 241 // 242

  private val v_start = 1 // 1
  private val v_end = 260 // 261
  private val v_x: Array[Int] = Array(av_x, bv_x, cv_x, dv_x)

  private val ah_y = 1 // 1
  private val bh_y = 65 // 66
  private val ch_y = 130 // 131
  private val dh_y = 195 // 196
  private val eh_y = 260 // 261

  private val h_start = 202 // 203
  private val h_end = 238 //239
  private val h_y: Array[Int] = Array(ah_y, bh_y, ch_y, dh_y, eh_y)

  private val ah_start_ll = new Point(184, 262)
  private val ah_end_ll = new Point(241, 263)
  private val bh_start_ll = new Point(241, 239)
  private val bh_end_ll = new Point(270, 239)
  private val ch_start_ll = new Point(222, 282)
  private val ch_end_ll = new Point(241, 282)
  private val dh_start_ll = new Point(184, 327)
  private val dh_end_ll = new Point(222, 327)
  private val eh_start_ll = new Point(222, 321)
  private val eh_end_ll = new Point(241, 321)
  private val fh_start_ll = new Point(241, 327)
  private val fh_end_ll = new Point(270, 327)
  private val gh_start_ll = new Point(184, 392)
  private val gh_end_ll = new Point(222, 392)

  private val h__ll: Array[(Point, Point)] = Array(
    (ah_start_ll, ah_end_ll),
    (bh_start_ll, bh_end_ll),
    (ch_start_ll, ch_end_ll),
    (dh_start_ll, dh_end_ll),
    (eh_start_ll, eh_end_ll),
    (fh_start_ll, fh_end_ll),
    (gh_start_ll, gh_end_ll)
  )

  private val hh_ll = new Point(183, 321)
  private val ih_ll = new Point(183, 366)
  private val jh_ll = new Point(223, 366)

  private val floor_one_between_corridors = new Point(220, 261)

  private val horizontal_long_wall_end_l = new Point(199, 261)
  private val horizontal_long_wall_end_r = new Point(241, 261)

  private val points: List[Point] = List(hh_ll, ih_ll, jh_ll, horizontal_long_wall_end_l, horizontal_long_wall_end_r, floor_one_between_corridors)
// walls low level
  private val av_start_ll = new Point(184, 262)
  private val av_end_ll = new Point(184, 392)
  private val bv_start_ll = new Point(222, 262)
  private val bv_end_ll = new Point(222, 282)
  private val cv_start_ll = new Point(241, 262)
  private val cv_end_ll = new Point(241, 282)
  private val dv_start_ll = new Point(270, 239)
  private val dv_end_ll = new Point(270, 327)
  private val ev_start_ll = new Point(241, 321)
  private val ev_end_ll = new Point(241, 327)
  private val fv_start_ll = new Point(224, 321)
  private val fv_end_ll = new Point(224, 366)
  private val gv_start_ll = new Point(222, 321)
  private val gv_end_ll = new Point(222, 392)
  private val hv_start_ll = new Point(182, 321)
  private val hv_end_ll = new Point(182, 366)

  private val v__ll: Array[(Point, Point)] = Array(
    (av_start_ll, av_end_ll),
    (bv_start_ll, bv_end_ll),
    (cv_start_ll, cv_end_ll),
    (dv_start_ll, dv_end_ll),
    (ev_start_ll, ev_end_ll),
    (fv_start_ll, fv_end_ll),
    (gv_start_ll, gv_end_ll),
    (hv_start_ll, hv_end_ll)
  )
  // d = door
  private val a_d_l = new Point(201, 32)
  private val a_d_r = new Point(239, 32)
  private val b_d_l = new Point(201, 97)
  private val b_d_r = new Point(239, 97)
  private val c_d_l = new Point(201, 162)
  private val c_d_r = new Point(239, 162)
  private val d_d_l = new Point(201, 227)
  private val d_d_r = new Point(239, 227)
  private val p_d_l = new Point(184, 322)
  private val o_d_r = new Point(222, 322)
  private val g_d_l = new Point(184, 365)
  private val j_d_r = new Point(220, 365)
  private val m_d_l = new Point(221, 262)
  private val n_d_r = new Point(219, 262)

  private val f_d = new Point(184, 272)
  private val e_d = new Point(194, 262)
  private val j_d1 = new Point(230, 282)
  private val j_d2 = new Point(231, 282)
  private val j_d3 = new Point(232, 282)
  private val i_d1 = new Point(230, 321)
  private val i_d2 = new Point(231, 321)
  private val i_d3 = new Point(232, 321)
  private val k_d1 = new Point(270, 299)
  private val k_d2 = new Point(270, 300)
  private val l_d = new Point(270, 259)

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
    p_d_l,
    o_d_r,
    g_d_l,
    j_d_r,
    m_d_l,
    n_d_r
  )
  val escapePoints: Array[Point] = Array(
    f_d,
    e_d,
    j_d1,
    j_d2,
    j_d3,
    i_d1,
    i_d2,
    i_d3,
    k_d1,
    k_d2,
    l_d
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

    for (i <- h__ll.indices)
      wallsPointsList = drawHorizontalLine(h__ll(i)._1, h__ll(i)._2, wallsPointsList)


    for (i <- v__ll.indices)
      wallsPointsList = drawVerticalLine(v__ll(i)._1, v__ll(i)._2, wallsPointsList)

    wallsPointsList = points ++ wallsPointsList


    // inside horizontal walls
    for (x <- RoomCornersWorker2.leftMiddleCornerA.x + 1 until RoomCornersWorker2.rightMiddleCornerA.x) {
      wallsPointsList = addPointToList(x, RoomCornersWorker2.leftMiddleCornerA.y + 1, wallsPointsList)
      wallsPointsList = addPointToList(x, RoomCornersWorker2.rightMiddleCornerA.y - 1, wallsPointsList)

      wallsPointsList = addPointToList(x, RoomCornersWorker2.leftMiddleCornerB.y + 1, wallsPointsList)
      wallsPointsList = addPointToList(x, RoomCornersWorker2.rightMiddleCornerB.y - 1, wallsPointsList)

      wallsPointsList = addPointToList(x, RoomCornersWorker2.leftMiddleCornerC.y + 1, wallsPointsList)
      wallsPointsList = addPointToList(x, RoomCornersWorker2.rightMiddleCornerC.y - 1, wallsPointsList)

      wallsPointsList = addPointToList(x, RoomCornersWorker2.leftMiddleCornerD.y + 1, wallsPointsList)
      wallsPointsList = addPointToList(x, RoomCornersWorker2.rightMiddleCornerD.y - 1, wallsPointsList)
    }

    // inside vertical walls
    for (y <- RoomCornersWorker2.leftMiddleCornerA.y + 1 until RoomCornersWorker2.rightMiddleCornerA.y) {
      wallsPointsList = addPointToList(RoomCornersWorker2.leftMiddleCornerA.x + 1, y, wallsPointsList)
      wallsPointsList = addPointToList(RoomCornersWorker2.rightMiddleCornerA.x - 1, y, wallsPointsList)
    }
    for (y <- RoomCornersWorker2.leftMiddleCornerB.y + 1 until RoomCornersWorker2.rightMiddleCornerB.y) {
      wallsPointsList = addPointToList(RoomCornersWorker2.leftMiddleCornerB.x + 1, y, wallsPointsList)
      wallsPointsList = addPointToList(RoomCornersWorker2.rightMiddleCornerB.x - 1, y, wallsPointsList)
    }
    for (y <- RoomCornersWorker2.leftMiddleCornerC.y + 1 until RoomCornersWorker2.rightMiddleCornerC.y) {
      wallsPointsList = addPointToList(RoomCornersWorker2.leftMiddleCornerC.x + 1, y, wallsPointsList)
      wallsPointsList = addPointToList(RoomCornersWorker2.rightMiddleCornerC.x - 1, y, wallsPointsList)
    }
    for (y <- RoomCornersWorker2.leftMiddleCornerD.y + 1 until RoomCornersWorker2.rightMiddleCornerD.y) {
      wallsPointsList = addPointToList(RoomCornersWorker2.leftMiddleCornerD.x + 1, y, wallsPointsList)
      wallsPointsList = addPointToList(RoomCornersWorker2.rightMiddleCornerD.x - 1, y, wallsPointsList)
    }

    wallsPointsList
  }

  private def addPointToList(x: Int, y: Int, list: List[Point]): List[Point] = {
    new Point(x, y) :: list
  }

  private def drawHorizontalLine(start: Point, end: Point, list: List[Point]): List[Point] = {
    var newList: List[Point] = list
    for (x <- start.x to end.x)
      newList = new Point(x, start.y) :: newList
    newList
  }

  private def drawVerticalLine(start: Point, end: Point, list: List[Point]): List[Point] = {
    var newList: List[Point] = list
    for (y <- start.y to end.y)
      newList = new Point(start.x, y) :: newList
    newList
  }
}