package evacuation.model.building

import evacuation.config.EvacuationConfig
import evacuation.model.EvacuationDirectionSmellStrength
import evacuation.model.EvacuationDirectionSmellStrength.EvacuationDirectionSmellStrength

import scala.util.Random

final class BuildingMap(implicit config: EvacuationConfig) {
  private val random = new Random(System.nanoTime())

  private val walls: Array[PointPair] = Array(
    // outside walls
    // first row
    // A
    new PointPair(new Point(1, 1), new Point(1, 65)),
    new PointPair(new Point(1, 65), new Point(38, 65)),
    new PointPair(new Point(38, 1), new Point(38, 65)),
    new PointPair(new Point(1, 1), new Point(38, 1)),
    //B
    new PointPair(new Point(1, 67), new Point(1, 131)),
    new PointPair(new Point(1, 131), new Point(38, 131)),
    new PointPair(new Point(38, 67), new Point(38, 131)),
    new PointPair(new Point(1, 67), new Point(38, 67)),
    // C
    new PointPair(new Point(1, 133), new Point(1, 197)),
    new PointPair(new Point(1, 197), new Point(38, 197)),
    new PointPair(new Point(38, 133), new Point(38, 197)),
    new PointPair(new Point(1, 133), new Point(38, 133)),
    // D
    new PointPair(new Point(1, 199), new Point(1, 263)),
    new PointPair(new Point(1, 263), new Point(38, 263)),
    new PointPair(new Point(38, 199), new Point(38, 263)),
    new PointPair(new Point(1, 199), new Point(38, 199)),

    // 2nd row
    // E
    new PointPair(new Point(40, 1), new Point(40, 65)),
    new PointPair(new Point(40, 65), new Point(77, 65)),
    new PointPair(new Point(77, 1), new Point(77, 65)),
    new PointPair(new Point(40, 1), new Point(77, 1)),
    // F
    new PointPair(new Point(40, 67), new Point(40, 131)),
    new PointPair(new Point(40, 131), new Point(77, 131)),
    new PointPair(new Point(77, 67), new Point(77, 131)),
    new PointPair(new Point(40, 67), new Point(77, 67)),
    // G
    new PointPair(new Point(40, 133), new Point(40, 197)),
    new PointPair(new Point(40, 197), new Point(77, 197)),
    new PointPair(new Point(77, 133), new Point(77, 197)),
    new PointPair(new Point(40, 133), new Point(77, 133)),
    // H
    new PointPair(new Point(40, 199), new Point(40, 263)),
    new PointPair(new Point(40, 263), new Point(77, 263)),
    new PointPair(new Point(77, 199), new Point(77, 263)),
    new PointPair(new Point(40, 199), new Point(77, 199)),

    // 3rd row
    // I
    new PointPair(new Point(79, 1), new Point(79, 65)),
    new PointPair(new Point(79, 65), new Point(116, 65)),
    new PointPair(new Point(116, 1), new Point(116, 65)),
    new PointPair(new Point(79, 1), new Point(116, 1)),
    // J
    new PointPair(new Point(79, 67), new Point(79, 131)),
    new PointPair(new Point(79, 131), new Point(116, 131)),
    new PointPair(new Point(116, 67), new Point(116, 131)),
    new PointPair(new Point(79, 67), new Point(116, 67)),
    // K
    new PointPair(new Point(79, 133), new Point(79, 197)),
    new PointPair(new Point(79, 197), new Point(116, 197)),
    new PointPair(new Point(116, 133), new Point(116, 197)),
    new PointPair(new Point(79, 133), new Point(116, 133)),

    // 4rd row
    // M
    new PointPair(new Point(118, 1), new Point(118, 65)),
    new PointPair(new Point(118, 65), new Point(155, 65)),
    new PointPair(new Point(155, 1), new Point(155, 65)),
    new PointPair(new Point(118, 1), new Point(155, 1)),

    // ground floor
    new PointPair(new Point(118, 176), new Point(118, 206)),
    new PointPair(new Point(118, 206), new Point(123, 206)),
    new PointPair(new Point(123, 206), new Point(123, 226)),
    new PointPair(new Point(118, 226), new Point(123, 226)),
    new PointPair(new Point(118, 226), new Point(118, 263)),
    new PointPair(new Point(118, 263), new Point(182, 263)),

    new PointPair(new Point(182, 226), new Point(82, 263)),
    new PointPair(new Point(157, 226), new Point(182, 226)),
    new PointPair(new Point(157, 206), new Point(157, 226)),
    new PointPair(new Point(157, 206), new Point(214, 206)),
    new PointPair(new Point(214, 176), new Point(214, 206)),
    new PointPair(new Point(118, 176), new Point(214, 176)),

    // do not let smell outside
    // A
    new PointPair(new Point(144, 175), new Point(147, 175)),
    // B
    new PointPair(new Point(122, 213), new Point(122, 219)),
    // D
    new PointPair(new Point(183, 252), new Point(183, 254)),
    // E
    new PointPair(new Point(158, 213), new Point(158, 219)),
    // F
    new PointPair(new Point(203, 175), new Point(205, 175)),

    // inside walls
    // first row
    // A
    new PointPair(new Point(13, 16), new Point(13, 50)),
    new PointPair(new Point(13, 50), new Point(26, 50)),
    new PointPair(new Point(26, 16), new Point(26, 50)),
    new PointPair(new Point(13, 16), new Point(26, 16)),
    //B
    new PointPair(new Point(13, 82), new Point(13, 116)),
    new PointPair(new Point(13, 116), new Point(26, 116)),
    new PointPair(new Point(26, 82), new Point(26, 116)),
    new PointPair(new Point(13, 82), new Point(26, 82)),
    // C
    new PointPair(new Point(13, 148), new Point(13, 182)),
    new PointPair(new Point(13, 182), new Point(26, 182)),
    new PointPair(new Point(26, 148), new Point(26, 182)),
    new PointPair(new Point(13, 148), new Point(26, 148)),
    // D
    new PointPair(new Point(13, 214), new Point(13, 248)),
    new PointPair(new Point(13, 248), new Point(26, 248)),
    new PointPair(new Point(26, 214), new Point(26, 248)),
    new PointPair(new Point(13, 214), new Point(26, 214)),

    // corridor A
    new PointPair(new Point(15, 16), new Point(15, 50)),
    new PointPair(new Point(24, 16), new Point(24, 50)),
    // corridor B
    new PointPair(new Point(15, 82), new Point(15, 116)),
    new PointPair(new Point(24, 82), new Point(24, 116)),
    // corridor C
    new PointPair(new Point(15, 148), new Point(15, 182)),
    new PointPair(new Point(24, 148), new Point(24, 182)),
    // corridor D
    new PointPair(new Point(15, 214), new Point(15, 248)),
    new PointPair(new Point(24, 214), new Point(24, 248)),

    // 2nd row
    // E
    new PointPair(new Point(52, 16), new Point(52, 50)),
    new PointPair(new Point(52, 50), new Point(65, 50)),
    new PointPair(new Point(65, 16), new Point(65, 50)),
    new PointPair(new Point(52, 16), new Point(65, 16)),
    // F
    new PointPair(new Point(52, 82), new Point(52, 116)),
    new PointPair(new Point(52, 116), new Point(65, 116)),
    new PointPair(new Point(65, 82), new Point(65, 116)),
    new PointPair(new Point(52, 82), new Point(65, 82)),
    // G
    new PointPair(new Point(52, 148), new Point(52, 182)),
    new PointPair(new Point(52, 182), new Point(65, 182)),
    new PointPair(new Point(65, 148), new Point(65, 182)),
    new PointPair(new Point(52, 148), new Point(65, 148)),
    // H
    new PointPair(new Point(52, 214), new Point(52, 248)),
    new PointPair(new Point(52, 248), new Point(65, 248)),
    new PointPair(new Point(65, 214), new Point(65, 248)),
    new PointPair(new Point(52, 214), new Point(65, 214)),

    // corridor E
    new PointPair(new Point(54, 16), new Point(54, 50)),
    new PointPair(new Point(63, 16), new Point(63, 50)),
    // corridor F
    new PointPair(new Point(54, 82), new Point(54, 116)),
    new PointPair(new Point(63, 82), new Point(63, 116)),
    // corridor G
    new PointPair(new Point(54, 148), new Point(54, 182)),
    new PointPair(new Point(63, 148), new Point(63, 182)),
    // corridor H
    new PointPair(new Point(54, 214), new Point(54, 248)),
    new PointPair(new Point(63, 214), new Point(63, 248)),

    // 3rd row
    // I
    new PointPair(new Point(91, 16), new Point(91, 50)),
    new PointPair(new Point(91, 50), new Point(104, 50)),
    new PointPair(new Point(104, 16), new Point(104, 50)),
    new PointPair(new Point(91, 16), new Point(104, 16)),
    // J
    new PointPair(new Point(91, 82), new Point(91, 116)),
    new PointPair(new Point(91, 116), new Point(104, 116)),
    new PointPair(new Point(104, 82), new Point(104, 116)),
    new PointPair(new Point(91, 82), new Point(104, 82)),
    // K
    new PointPair(new Point(91, 148), new Point(91, 182)),
    new PointPair(new Point(91, 182), new Point(104, 182)),
    new PointPair(new Point(104, 148), new Point(104, 182)),
    new PointPair(new Point(91, 148), new Point(104, 148)),

    // corridor I
    new PointPair(new Point(93, 16), new Point(93, 50)),
    new PointPair(new Point(102, 16), new Point(102, 50)),
    // corridor J
    new PointPair(new Point(93, 82), new Point(93, 116)),
    new PointPair(new Point(102, 82), new Point(102, 116)),
    // corridor K
    new PointPair(new Point(93, 148), new Point(93, 182)),
    new PointPair(new Point(102, 148), new Point(102, 182)),

    // 4rd row
    // M
    new PointPair(new Point(130, 16), new Point(130, 50)),
    new PointPair(new Point(130, 50), new Point(143, 50)),
    new PointPair(new Point(143, 16), new Point(143, 50)),
    new PointPair(new Point(130, 16), new Point(143, 16)),

    //corridor M
    new PointPair(new Point(132, 16), new Point(132, 50)),
    new PointPair(new Point(141, 16), new Point(141, 50)),

    //ground floor
    new PointPair(new Point(133, 238), new Point(138, 238)),
    new PointPair(new Point(133, 238), new Point(133, 251)),
    new PointPair(new Point(133, 251), new Point(138, 251)),

    new PointPair(new Point(162, 238), new Point(167, 238)),
    new PointPair(new Point(167, 238), new Point(167, 251)),
    new PointPair(new Point(162, 251), new Point(167, 251))
  )

  private object smellSources {
    val A_a = new Point(14,  17)
    val A_b = new Point(25,  49)
    val B_a = new Point(14,  83)
    val B_b = new Point(25, 115)
    val C_a = new Point(14, 149)
    val C_b = new Point(25, 181)
    val D_a = new Point(14, 215)
    val D_b = new Point(25, 247)

    val E_a = new Point(53,  17)
    val E_b = new Point(64,  49)
    val F_a = new Point(53,  83)
    val F_b = new Point(64, 115)
    val G_a = new Point(53, 149)
    val G_b = new Point(64, 181)
    val H_a = new Point(53, 215)
    val H_b = new Point(64, 247)

    val I_a = new Point(92,   17)
    val I_b = new Point(103,  49 )
    val J_a = new Point(92,   83 )
    val J_b = new Point(103, 115 )
    val K_a = new Point(92,  149 )
    val K_b = new Point(103, 181 )

    val M_a = new Point(131, 17)
    val M_b = new Point(142, 49)
  }

  private object teleportationDestination {
    val A_a = new Point(14, 115)
    val A_b = new Point(25, 83 )
    val B_a = new Point(14, 181)
    val B_b = new Point(25, 149)
    val C_a = new Point(14, 247)
    val C_b = new Point(25, 215)
    val D_a = new Point(53, 49)
    val D_b = new Point(64, 17)

    val E_a = new Point(53, 115)
    val E_b = new Point(64, 83 )
    val F_a = new Point(53, 181)
    val F_b = new Point(64, 149)
    val G_a = new Point(53, 247)
    val G_b = new Point(64, 215)
    val H_a = new Point(92, 49)
    val H_b = new Point(103, 17)

    val I_a = new Point(92, 115)
    val I_b = new Point(103,83 )
    val J_a = new Point(92, 181)
    val J_b = new Point(103,149)
    val K_a = new Point(134, 250) // on 1st floor
    val K_b = new Point(166, 250) // on 1st floor

    val M_a = new Point(134, 239) // on 1st floor
    val M_b = new Point(166, 239) // on 1st floor
  }

  private object doors {
    val A_a = new Point(13,  21)
    val A_b = new Point(26,  45)
    val B_a = new Point(13,  87)
    val B_b = new Point(26, 111)
    val C_a = new Point(13, 153)
    val C_b = new Point(26, 177)
    val D_a = new Point(13, 219)
    val D_b = new Point(26, 243)

    val E_a = new Point(52,  21)
    val E_b = new Point(65,  45)
    val F_a = new Point(52,  87)
    val F_b = new Point(65, 111)
    val G_a = new Point(52, 153)
    val G_b = new Point(65, 177)
    val H_a = new Point(52, 219)
    val H_b = new Point(65, 243)

    val I_a = new Point(91,   21)
    val I_b = new Point(104,  45)
    val J_a = new Point(91,   87)
    val J_b = new Point(104, 111)
    val K_a = new Point(91,  153)
    val K_b = new Point(104, 177)

    val M_a = new Point(130, 21)
    val M_b = new Point(143, 45)
  }

  private object rectanglesCornersDrawingAvailableSpaceOnFloors {
    val floorA: Array[PointPair] = Array(
      new PointPair(new Point(2,2), new Point(12, 64)),
      new PointPair(new Point(13, 2), new Point(26, 15)),
      new PointPair(new Point(13, 51), new Point(26, 64)),
      new PointPair(new Point(27, 2), new Point(37, 64))
    )
    val floorB: Array[PointPair] = Array(
      new PointPair(new Point(2, 68), new Point(12, 130)),
      new PointPair(new Point(13, 68), new Point(26, 81)),
      new PointPair(new Point(13, 117), new Point(26, 130)),
      new PointPair(new Point(27, 68), new Point(37, 130))
    )
    val floorC: Array[PointPair] = Array(
      new PointPair(new Point(2,134), new Point(12, 196)),
      new PointPair(new Point(13, 134), new Point(26, 147)),
      new PointPair(new Point(13, 183), new Point(26, 196)),
      new PointPair(new Point(27, 134), new Point(37, 196))
    )
    val floorD: Array[PointPair] = Array(
      new PointPair(new Point(2,200), new Point(12, 262)),
      new PointPair(new Point(13, 200), new Point(26, 213)),
      new PointPair(new Point(13, 249), new Point(26, 262)),
      new PointPair(new Point(27, 200), new Point(37, 262))
    )
    val floorE: Array[PointPair] = Array(
      new PointPair(new Point(41,2), new Point(51, 64)),
      new PointPair(new Point(50, 2), new Point(65, 15)),
      new PointPair(new Point(50, 51), new Point(65, 64)),
      new PointPair(new Point(66, 2), new Point(76, 64))
    )
    val floorF: Array[PointPair] = Array(
      new PointPair(new Point(41, 68), new Point(51, 130)),
      new PointPair(new Point(50, 68), new Point(65, 81)),
      new PointPair(new Point(50, 117), new Point(65, 130)),
      new PointPair(new Point(66, 68), new Point(76, 130))
    )
    val floorG: Array[PointPair] = Array(
      new PointPair(new Point(41,134), new Point(51, 196)),
      new PointPair(new Point(50, 134), new Point(65, 147)),
      new PointPair(new Point(50, 183), new Point(65, 196)),
      new PointPair(new Point(66, 134), new Point(76, 196))
    )
    val floorH: Array[PointPair] = Array(
      new PointPair(new Point(41,200), new Point(51, 262)),
      new PointPair(new Point(50, 200), new Point(65, 213)),
      new PointPair(new Point(50, 249), new Point(65, 262)),
      new PointPair(new Point(66, 200), new Point(76, 262))
    )
    val floorI: Array[PointPair] = Array(
      new PointPair(new Point(80,2), new Point(90, 64)),
      new PointPair(new Point(91, 2), new Point(104, 15)),
      new PointPair(new Point(91, 51), new Point(104, 64)),
      new PointPair(new Point(105, 2), new Point(115, 64))
    )
    val floorJ: Array[PointPair] = Array(
      new PointPair(new Point(80, 68), new Point(90, 130)),
      new PointPair(new Point(91, 68), new Point(104, 81)),
      new PointPair(new Point(91, 117), new Point(104, 130)),
      new PointPair(new Point(105, 68), new Point(115, 130))
    )
    val floorK: Array[PointPair] = Array(
      new PointPair(new Point(80,134), new Point(90, 196)),
      new PointPair(new Point(91, 134), new Point(104, 147)),
      new PointPair(new Point(91, 183), new Point(104, 196)),
      new PointPair(new Point(105, 134), new Point(115, 196))
    )
    val floorM: Array[PointPair] = Array(
      new PointPair(new Point(119,2), new Point(129, 64)),
      new PointPair(new Point(130, 2), new Point(143, 15)),
      new PointPair(new Point(130, 51), new Point(143, 64)),
      new PointPair(new Point(144, 2), new Point(154, 64))
    )
  }

  private object rectanglesCornersDrawingAvailableSpaceOnFloor1 {
    val floorZ: Array[PointPair] = Array(
      new PointPair(new Point(119,177), new Point(213, 205)),
      new PointPair(new Point(124, 206), new Point(156, 237)),
      new PointPair(new Point(119, 227), new Point(123, 262)),
      new PointPair(new Point(170, 227), new Point(181, 262))
    )
  }

  val wallsPoints: List[Point] = getWallsPoints

  val smellOrigins: Array[Point] = Array(
    smellSources.A_a,
    smellSources.A_b,
    smellSources.B_a,
    smellSources.B_b,
    smellSources.C_a,
    smellSources.C_b,
    smellSources.D_a,
    smellSources.D_b,
    smellSources.E_a,
    smellSources.E_b,
    smellSources.F_a,
    smellSources.F_b,
    smellSources.G_a,
    smellSources.G_b,
    smellSources.H_a,
    smellSources.H_b,
    smellSources.I_a,
    smellSources.I_b,
    smellSources.J_a,
    smellSources.J_b,
    smellSources.K_a,
    smellSources.K_b,
    smellSources.M_a,
    smellSources.M_b
  )

  val doorsPoints: Array[Point] = Array(
    doors.A_a,
    doors.A_b,
    doors.B_a,
    doors.B_b,
    doors.C_a,
    doors.C_b,
    doors.D_a,
    doors.D_b,
    doors.E_a,
    doors.E_b,
    doors.F_a,
    doors.F_b,
    doors.G_a,
    doors.G_b,
    doors.H_a,
    doors.H_b,
    doors.I_a,
    doors.I_b,
    doors.J_a,
    doors.J_b,
    doors.K_a,
    doors.K_b,
    doors.M_a,
    doors.M_b
  )

  val peoplePoints: List[Point] = getPeoplePoints
  val peoplePointsOnFloor1: List[Point] = getPeoplePointsOnFloor1

  val teleportationPairs: Array[PointPair] = Array(
    new PointPair(smellSources.A_a, teleportationDestination.A_a),
    new PointPair(smellSources.A_b, teleportationDestination.A_b),
    new PointPair(smellSources.B_a, teleportationDestination.B_a),
    new PointPair(smellSources.B_b, teleportationDestination.B_b),
    new PointPair(smellSources.C_a, teleportationDestination.C_a),
    new PointPair(smellSources.C_b, teleportationDestination.C_b),
    new PointPair(smellSources.D_a, teleportationDestination.D_a),
    new PointPair(smellSources.D_b, teleportationDestination.D_b),
    new PointPair(smellSources.E_a, teleportationDestination.E_a),
    new PointPair(smellSources.E_b, teleportationDestination.E_b),
    new PointPair(smellSources.F_a, teleportationDestination.F_a),
    new PointPair(smellSources.F_b, teleportationDestination.F_b),
    new PointPair(smellSources.G_a, teleportationDestination.G_a),
    new PointPair(smellSources.G_b, teleportationDestination.G_b),
    new PointPair(smellSources.H_a, teleportationDestination.H_a),
    new PointPair(smellSources.H_b, teleportationDestination.H_b),
    new PointPair(smellSources.I_a, teleportationDestination.I_a),
    new PointPair(smellSources.I_b, teleportationDestination.I_b),
    new PointPair(smellSources.J_a, teleportationDestination.J_a),
    new PointPair(smellSources.J_b, teleportationDestination.J_b),
    new PointPair(smellSources.K_a, teleportationDestination.K_a),
    new PointPair(smellSources.K_b, teleportationDestination.K_b),
    new PointPair(smellSources.M_a, teleportationDestination.M_a),
    new PointPair(smellSources.M_b, teleportationDestination.M_b)
  )

  val exits: Array[(Point, EvacuationDirectionSmellStrength)] = Array(
    // A
    (new Point(145, 176), EvacuationDirectionSmellStrength.Weak),
    (new Point(146, 176), EvacuationDirectionSmellStrength.Strong),

    // B
    (new Point(123, 214), EvacuationDirectionSmellStrength.Weak),
    (new Point(123, 215), EvacuationDirectionSmellStrength.Weak),
    (new Point(123, 216), EvacuationDirectionSmellStrength.Weak),
    (new Point(123, 217), EvacuationDirectionSmellStrength.Weak),

    // C
    (new Point(157, 263), EvacuationDirectionSmellStrength.Weak),

    // D
    (new Point(182, 253), EvacuationDirectionSmellStrength.Strong),

    //E
    (new Point(157, 214), EvacuationDirectionSmellStrength.Strong),
    (new Point(157, 215), EvacuationDirectionSmellStrength.Weak),
    (new Point(157, 216), EvacuationDirectionSmellStrength.Weak),
    (new Point(157, 217), EvacuationDirectionSmellStrength.Weak),

    // F
    (new Point(204, 176), EvacuationDirectionSmellStrength.Strong)
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
      getPeopleOnFloor(rectanglesCornersDrawingAvailableSpaceOnFloors.floorA, config.peopleNoFloorA) ++
        getPeopleOnFloor(rectanglesCornersDrawingAvailableSpaceOnFloors.floorB, config.peopleNoFloorB) ++
        getPeopleOnFloor(rectanglesCornersDrawingAvailableSpaceOnFloors.floorC, config.peopleNoFloorC) ++
        getPeopleOnFloor(rectanglesCornersDrawingAvailableSpaceOnFloors.floorD, config.peopleNoFloorD) ++
        getPeopleOnFloor(rectanglesCornersDrawingAvailableSpaceOnFloors.floorE, config.peopleNoFloorE) ++
        getPeopleOnFloor(rectanglesCornersDrawingAvailableSpaceOnFloors.floorF, config.peopleNoFloorF) ++
        getPeopleOnFloor(rectanglesCornersDrawingAvailableSpaceOnFloors.floorG, config.peopleNoFloorG) ++
        getPeopleOnFloor(rectanglesCornersDrawingAvailableSpaceOnFloors.floorH, config.peopleNoFloorH) ++
        getPeopleOnFloor(rectanglesCornersDrawingAvailableSpaceOnFloors.floorI, config.peopleNoFloorI) ++
        getPeopleOnFloor(rectanglesCornersDrawingAvailableSpaceOnFloors.floorJ, config.peopleNoFloorJ) ++
        getPeopleOnFloor(rectanglesCornersDrawingAvailableSpaceOnFloors.floorK, config.peopleNoFloorK) ++
        getPeopleOnFloor(rectanglesCornersDrawingAvailableSpaceOnFloors.floorM, config.peopleNoFloorM)
  }

  private def getPeoplePointsOnFloor1: List[Point] = {
      getPeopleOnFloor(rectanglesCornersDrawingAvailableSpaceOnFloor1.floorZ, config.peopleNoFloorZ)
  }

  private def getPeopleOnFloor(floorParts: Array[PointPair], noOfPeople: Int): List[Point] = {
    var result: List[Point] = List.empty

    for (_ <- 0 until noOfPeople) {
      val randomFloorPart = floorParts(random.nextInt(4))

      val randomPointX = random.nextInt(randomFloorPart.point2.x - randomFloorPart.point1.x + 1) + randomFloorPart.point1.x
      val randomPointY = random.nextInt(randomFloorPart.point2.y - randomFloorPart.point1.y + 1) + randomFloorPart.point1.y

      result = new Point(randomPointY, randomPointX) :: result
    }

    result
  }
}