package evacuation.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class EvacuationMetrics() extends Metrics {
  override def log: String = {
    ""
  }
  override def series: Vector[(String, Double)] = Vector()

  override def +(other: Metrics): EvacuationMetrics = {
    other match {
      case EvacuationMetrics.EMPTY => this
      case EvacuationMetrics() => EvacuationMetrics()
      case null => this
      case _ => throw new UnsupportedOperationException(s"Problem with adding metrics")
    }
  }
}

object EvacuationMetrics {
  private val EMPTY = EvacuationMetrics()

  def empty(): EvacuationMetrics = EMPTY
}