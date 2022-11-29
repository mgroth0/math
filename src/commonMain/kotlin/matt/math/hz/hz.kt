package matt.math.hz

import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import matt.model.data.mathable.DoubleWrapper
import matt.model.data.num.NumberWrapper
import matt.model.data.sensemod.Phase
import matt.model.data.sensemod.WaveForm
import kotlin.time.Duration.Companion.seconds

val Number.hz get() = Hz(toDouble())

/*SHOULD BE VALUE CLASS*/

object HzSerializer: KSerializer<Hz> {
  override val descriptor: SerialDescriptor
	get() = PrimitiveSerialDescriptor("Hz", PrimitiveKind.DOUBLE)

  override fun deserialize(decoder: Decoder): Hz {
	return Hz(decoder.decodeDouble())
  }

  override fun serialize(encoder: Encoder, value: Hz) {
	encoder.encodeDouble(value.asDouble)
  }

}

@Serializable(with = HzSerializer::class)
data class Hz(override val asNumber: Double): DoubleWrapper<Hz>, NumberWrapper {
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
  val enabled: Boolean = true /*temp stupid*/
)

@Serializable
data class SubWaveConfig(
  val form: WaveForm,
  val phase: Phase,
  val freq: Hz,
  val minAnalog: Int = 0
)
