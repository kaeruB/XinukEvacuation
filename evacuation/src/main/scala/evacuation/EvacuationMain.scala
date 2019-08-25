package evacuation

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import evacuation.algorithm.EvacuationMovesController
import evacuation.model.{DangerCell, EvacuationDirectionCell, PersonCell, StaircaseCell, WallCell}
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, Obstacle, SmellingCell}
import evacuation.model.parallel.EvacuationConflictResolver

object EvacuationMain extends LazyLogging {
  private val configPrefix = "evacuation"
  private val metricHeaders = Vector(
    "Number of people escaped through door 0"
  )

  def colorSmell(cell: SmellingCell): Color = {
    val smellValue = cell.smell.map(_.map(_.value).max).max.toFloat
    if (smellValue > 0.015) Color.RED
    else if (smellValue < 0) Color.BLUE
    else Color.BLACK
  }

  private def cellToColor(cell: SmellingCell): Color = {
    cell match {
      case PersonCell(_, _) => Color.WHITE
      case DangerCell(_) => Color.RED
      case EvacuationDirectionCell(_) => Color.BLUE
      case StaircaseCell(_) => Color.BLUE
     // case WallCell(_) => Color.YELLOW
      case cell: SmellingCell => colorSmell(cell)
      case _ => Color.BLACK
    }
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(configPrefix, metricHeaders, EvacuationConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard)(new EvacuationMovesController(_)(_),
    {
      case cell: SmellingCell => cellToColor(cell)
      case Obstacle => Color.yellow
    }
    ).start()
  }
}