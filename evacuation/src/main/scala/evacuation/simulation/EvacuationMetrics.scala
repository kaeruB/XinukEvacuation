package evacuation.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class EvacuationMetrics(
                                  evacuatedThroughDoor0Count: Long,
                                  evacuatedThroughDoor1Count: Long,
                                  evacuatedThroughDoor2Count: Long,
                                  evacuatedThroughDoor3Count: Long,
                                  evacuatedThroughDoor4Count: Long,
                                  evacuatedThroughDoor5Count: Long
                                  ) extends Metrics {
  override def log: String = {
    s"$evacuatedThroughDoor0Count, $evacuatedThroughDoor1Count, $evacuatedThroughDoor2Count, $evacuatedThroughDoor3Count, $evacuatedThroughDoor4Count, $evacuatedThroughDoor5Count"
  }
  override def series: Vector[(String, Double)] = Vector(
    "evacuatedThroughDoor0Count" -> evacuatedThroughDoor0Count,
    "evacuatedThroughDoor1Count" -> evacuatedThroughDoor1Count,
    "evacuatedThroughDoor2Count" -> evacuatedThroughDoor2Count,
    "evacuatedThroughDoor3Count" -> evacuatedThroughDoor3Count,
    "evacuatedThroughDoor4Count" -> evacuatedThroughDoor4Count,
    "evacuatedThroughDoor5Count" -> evacuatedThroughDoor5Count
  )

  override def +(other: Metrics): EvacuationMetrics = {
    other match {
      case EvacuationMetrics.EMPTY => this
      case EvacuationMetrics(
        otherEvacuatedThroughDoor0Count,
        otherEvacuatedThroughDoor1Count,
        otherEvacuatedThroughDoor2Count,
        otherEvacuatedThroughDoor3Count,
        otherEvacuatedThroughDoor4Count,
        otherEvacuatedThroughDoor5Count
      ) =>
        EvacuationMetrics(
          evacuatedThroughDoor0Count + otherEvacuatedThroughDoor0Count,
          evacuatedThroughDoor1Count + otherEvacuatedThroughDoor1Count,
          evacuatedThroughDoor2Count + otherEvacuatedThroughDoor2Count,
          evacuatedThroughDoor3Count + otherEvacuatedThroughDoor3Count,
          evacuatedThroughDoor4Count + otherEvacuatedThroughDoor4Count,
          evacuatedThroughDoor5Count + otherEvacuatedThroughDoor5Count
        )
      case null => this
      case _ => throw new UnsupportedOperationException(s"Problem with adding metrics")
    }
  }
}

object EvacuationMetrics {
  private val EMPTY = EvacuationMetrics(0, 0, 0, 0, 0, 0)

  def empty(): EvacuationMetrics = EMPTY
}