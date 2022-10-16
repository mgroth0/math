package matt.math.mathable


interface Mathable<M: Mathable<M>> {
  operator fun div(n: Number): M
  operator fun div(m: M): Number
  operator fun times(n: Number): M
  operator fun plus(m: M): M
  operator fun minus(m: M): M
}

interface MathAndComparable<M: MathAndComparable<M>>: Mathable<M>, Comparable<M>

interface DoubleWrapper<M: DoubleWrapper<M>>: MathAndComparable<M> {
  override fun compareTo(other: M): Int {
	return asDouble.compareTo(other.asDouble)
  }

  fun fromDouble(d: Double): M
  val asDouble: Double
  override fun plus(m: M): M {
	return fromDouble(asDouble + m.asDouble)
  }

  override fun minus(m: M): M {
	return fromDouble(asDouble - m.asDouble)
  }

  override fun div(n: Number): M {
	return fromDouble(asDouble/n.toDouble())
  }

  override fun times(n: Number): M {
	return fromDouble(asDouble*n.toDouble())
  }

  override fun div(m: M): Number {
	return asDouble/m.asDouble
  }

}


interface FloatWrapper<M: FloatWrapper<M>>: MathAndComparable<M> {
  override fun compareTo(other: M): Int {
	return asFloat.compareTo(other.asFloat)
  }

  fun fromFloat(d: Float): M
  val asFloat: Float
  override fun plus(m: M): M {
	return fromFloat(asFloat + m.asFloat)
  }

  override fun minus(m: M): M {
	return fromFloat(asFloat - m.asFloat)
  }

  override fun div(m: M): Number {
	return asFloat/m.asFloat
  }

  override fun div(n: Number): M {
	return fromFloat(asFloat/n.toFloat())
  }

  override fun times(n: Number): M {
	return fromFloat(asFloat*n.toFloat())
  }

}


