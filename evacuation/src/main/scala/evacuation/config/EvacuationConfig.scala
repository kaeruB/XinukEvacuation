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
                                   personsNumber: Int,
                                   personsNumberFloor1: Int,
                                   personsNumberFloor2: Int,
                                   floorHorizontalWidth: Int,
                                   floorVerticalWidth: Int,
                                   floorNumber: Int,
                                   intermediateDoorNumber: Int,
                                   escapeDoorNumber: Int
                                 )extends XinukConfig