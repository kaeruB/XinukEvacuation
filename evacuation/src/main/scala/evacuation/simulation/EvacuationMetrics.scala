package evacuation.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class EvacuationMetrics(
                                  evacuatedCount: Long
                                  ) extends Metrics {
  override def log: String = {
    s"$evacuatedCount"
  }
  override def series: Vector[(String, Double)] = Vector(
    "evacuatedCount" -> evacuatedCount
  )

  override def +(other: Metrics): EvacuationMetrics = {
    other match {
      case EvacuationMetrics.EMPTY => this
      case EvacuationMetrics(otherEvacuatedCount) =>
        EvacuationMetrics(evacuatedCount + otherEvacuatedCount)
      case null => this
      case _ => throw new UnsupportedOperationException(s"Problem with adding metrics")
    }
  }
}

object EvacuationMetrics {
  private val EMPTY = EvacuationMetrics(0)

  def empty(): EvacuationMetrics = EMPTY
}