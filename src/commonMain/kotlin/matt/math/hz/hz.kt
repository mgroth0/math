package matt.math.hz

import kotlinx.serialization.Serializable
import matt.math.mathable.DoubleWrapper
import matt.model.num.NumberWrapper
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
