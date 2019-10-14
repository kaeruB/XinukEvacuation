package evacuation.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
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
                                   personsNumberLower: Int,
                                   personsNumberFloor1: Int,
                                   personsNumberFloor2: Int,
                                   personsNumberFloor3: Int,
                                   personsNumberFloor4: Int,
                                   personsNumberFloor5: Int,
                                   personsNumberFloor6: Int,
                                   personsNumberFloor7: Int,
                                   personsNumberFloor8: Int,
                                   personsNumberFloor9: Int,
                                   personsNumberFloor10: Int,
                                   personsNumberFloor11: Int,
                                   personsNumberFloor12: Int,
                                   intermediateDoorNumber: Int,
                                   escapeDoorNumber: Int,
                                   crossBendFactor: Double,
                                   straightBendFactor: Double
                                 )extends XinukConfig