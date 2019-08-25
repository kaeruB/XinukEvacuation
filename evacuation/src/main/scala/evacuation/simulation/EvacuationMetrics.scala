package evacuation.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class EvacuationMetrics(
                                    evacuatedThroughDoor0Count: Long
                                  ) extends Metrics {
  override def log: String = {
    s"$evacuatedThroughDoor0Count"
  }
  override def series: Vector[(String, Double)] = Vector(
    "evacuatedThroughDoor0Count" -> evacuatedThroughDoor0Count
  )

  override def +(other: Metrics): EvacuationMetrics = {
    other match {
      case EvacuationMetrics.EMPTY => this
      case EvacuationMetrics(otherEvacuatedThroughDoor0Count) =>
        EvacuationMetrics(evacuatedThroughDoor0Count + otherEvacuatedThroughDoor0Count)
      case null => this
      case _ => throw new UnsupportedOperationException(s"Problem with adding metrics")
    }
  }
}

object EvacuationMetrics {
  private val EMPTY = EvacuationMetrics(0)

  def empty(): EvacuationMetrics = EMPTY
}