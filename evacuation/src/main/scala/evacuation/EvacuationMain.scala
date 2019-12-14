package evacuation

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import evacuation.algorithm.EvacuationMovesController
import evacuation.config.EvacuationConfig
import evacuation.model.{EvacuationDirectionCell, ExitCell, PersonCell}
import pl.edu.agh.xinuk.SimulationWithWind
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{CurvedSmellPropagation, DefaultSmellPropagation, Obstacle, SmellingCell}
import evacuation.model.parallel.EvacuationConflictResolver

object EvacuationMain extends LazyLogging {
  private val configPrefix = "evacuation"
  private val metricHeaders = Vector(
    "evacuatedCount"
  )

  def colorSmell(cell: SmellingCell): Color = {
    val smellValue = cell.smell.map(_.map(_.value).max).max.toFloat

    val brightness = Math.pow(smellValue, 0.1).toFloat
    if (smellValue < 0.00001) {
      val hue = 1f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else if (smellValue < 0.001) {
      val hue = 0.65f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else if (smellValue < 0.1) {
      val hue = 0.28f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else {
      val hue = 0.11f
      val saturation = 0.69f
      Color.getHSBColor(hue, saturation, brightness)
    }
  }

  private def cellToColor(cell: SmellingCell): Color = {
    cell match {
      case PersonCell(_) => Color.WHITE
      case EvacuationDirectionCell(_, _, _) => Color.BLUE
      case ExitCell(_, _) => Color.BLUE
      case cell: SmellingCell => colorSmell(cell)
      case _ => Color.BLACK
    }
  }

  def main(args: Array[String]): Unit = {

    def withWind(): Unit = {
      import pl.edu.agh.xinuk.config.ValueReaders._
      new SimulationWithWind[EvacuationConfig](
        configPrefix,
        metricHeaders,
        EvacuationConflictResolver,
        CurvedSmellPropagation.calculateSmellAddends
      )(
        (tuples, evacuationConfig) => new EvacuationMovesController(tuples)(evacuationConfig),
        {
          case cell: SmellingCell => cellToColor(cell)
          case Obstacle => Color.yellow
        }
      ).start()
    }

    def withoutWind(): Unit = {
      import pl.edu.agh.xinuk.config.ValueReaders._
      new Simulation(
        configPrefix,
        metricHeaders,
        EvacuationConflictResolver,
        DefaultSmellPropagation.calculateSmellAddendsStandard
      )(
        new EvacuationMovesController(_)(_),
        {
          case cell: SmellingCell => cellToColor(cell)
          case Obstacle => Color.yellow
        }
      ).start()
    }

    // withoutWind()
    withWind()
  }
}