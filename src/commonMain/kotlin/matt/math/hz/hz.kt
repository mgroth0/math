package matt.math.hz

import kotlinx.serialization.Serializable
import matt.math.mathable.DoubleWrapper
import kotlin.jvm.JvmInline
import kotlin.time.Duration.Companion.seconds
import matt.model.num.NumberWrapper

@Serializable @JvmInline value class Hz(override val asNumber: Double): DoubleWrapper<Hz>, NumberWrapper {
  override val asDouble get() = asNumber
  override fun fromDouble(d: Double): Hz {
	return Hz(d)
  }

  override fun toString(): String {
	return "${asNumber}matt.math.hz.Hz"
  }


  val interval get() = (1.seconds/asNumber)
}
