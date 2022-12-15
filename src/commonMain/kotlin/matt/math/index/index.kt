package matt.math.index

import kotlinx.serialization.Serializable
import matt.model.data.mathable.IntWrapper
import matt.model.op.convert.Converter
import kotlin.math.absoluteValue


/*should be a value class*/
@Serializable data class Index(val i: Int): IntWrapper<Index> {
  override fun fromInt(d: Int): Index {
	return Index(i)
  }

  override val asInt: Int
	get() = i

  override val abs: Index
	get() = Index(i.absoluteValue)
}

object IndexWrapperConverter: Converter<Index, Double> {
  override fun convertToB(a: Index): Double {
	return a.asInt.toDouble()
  }

  override fun convertToA(b: Double): Index {
	return Index(b.toInt())
  }

}

object IndexWrapperIntConverter: Converter<Index, Int> {
  override fun convertToB(a: Index): Int {
	return a.asInt
  }

  override fun convertToA(b: Int): Index {
	return Index(b)
  }

}