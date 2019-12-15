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
                                   evacuationDirectionInitialSignal: Signal,
                                   evacuationDirectionInitialSignalMedium: Signal,
                                   evacuationDirectionInitialSignalWeak: Signal,
                                   crossBendFactor: Double,
                                   straightBendFactor: Double,
                                   peopleNoFloorA: Int,
                                   peopleNoFloorB: Int,
                                   peopleNoFloorC: Int,
                                   peopleNoFloorD: Int,
                                   peopleNoFloorE: Int,
                                   peopleNoFloorF: Int,
                                   peopleNoFloorG: Int,
                                   peopleNoFloorH: Int,
                                   peopleNoFloorI: Int,
                                   peopleNoFloorJ: Int,
                                   peopleNoFloorK: Int,
                                   peopleNoFloorL: Int,
                                   peopleNoFloorM: Int,
                                   peopleNoFloorZ: Int
                                 )extends XinukConfigWithBendFactors