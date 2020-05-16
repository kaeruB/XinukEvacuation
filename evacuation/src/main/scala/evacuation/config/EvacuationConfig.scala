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
                                   peopleNoICloakroom: Int,
                                   peopleNoICorridor: Int,
                                   peopleNoII241: Int,
                                   peopleNoIICorridor: Int,
                                   peopleNoIII323: Int,
                                   peopleNoIII324: Int,
                                   peopleNoIII327a: Int,
                                   peopleNoIII327b: Int,
                                   peopleNoIII327c: Int,
                                   peopleNoIII327d: Int,
                                   peopleNoIII327e: Int,
                                   peopleNoIIICorridor: Int,
                                   peopleNoIIICorridorFast: Int,
                                   peopleNoIV426: Int,
                                   peopleNoIV428: Int,
                                   peopleNoIV429: Int,
                                   peopleNoIV430: Int,
                                   peopleNoIV431: Int,
                                   peopleNoIVCorridor: Int
                                 )extends XinukConfigWithBendFactors
