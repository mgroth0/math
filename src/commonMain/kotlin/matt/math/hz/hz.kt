package matt.math.hz

import kotlinx.serialization.Serializable
import matt.model.data.mathable.DoubleWrapper
import matt.model.data.num.NumberWrapper
import matt.model.data.sensemod.Phase
import matt.model.data.sensemod.WaveForm
import kotlin.jvm.JvmInline
import kotlin.time.Duration.Companion.seconds

val Number.hz get() = Hz(toDouble())

@Serializable @JvmInline value class Hz(override val asNumber: Double): DoubleWrapper<Hz>, NumberWrapper {
  override val asDouble get() = asNumber
  override fun fromDouble(d: Double): Hz {
	return Hz(d)
  }

  override fun toString(): String {
	return "${asNumber}Hz"
  }


  val interval get() = (1.seconds/asNumber)
}


@Serializable
data class WaveConfig(
  val carrierCfg: SubWaveConfig,
  val modulatorCfg: SubWaveConfig?,
  val enabled: Boolean /*temp stupid*/
)

@Serializable
data class SubWaveConfig(
  val form: WaveForm,
  val phase: Phase,
  val freq: Hz,
  val minAnalog: Int = 0
)
