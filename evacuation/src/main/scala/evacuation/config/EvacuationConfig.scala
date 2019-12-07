package evacuation.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfigWithBendFactors}
import pl.edu.agh.xinuk.model.Signal

final case class EvacuationConfig(
                                   signalSpeedRatio: Int,
                                   signalSuppressionFactor: Double,
                                   signalAttenuationFactor: Double,
                                   gridSize: Int,
                                   guiType: GuiType,
                                   guiCellSize: Int,
                                   workersRoot: Int,
                                   iterationsNumber: Long,
                                   isSupervisor: Boolean,
                                   shardingMod: Int,
                                   personInitialSignal: Signal,
                                   dangerInitialSignal: Signal,
                                   wallInitialSignal: Signal,
                                   evacuationDirectionInitialSignal: Signal,
                                   zeroInitialSignal: Signal,
                                   crossBendFactor: Double,
                                   straightBendFactor: Double,
                                   peopleNoFloor1: Int,
                                   peopleNoFloor2: Int
                                 )extends XinukConfigWithBendFactors