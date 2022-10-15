package matt.math.mathable

interface Mathable<M: Mathable<M>> {
  operator fun plus(m: M): M
  operator fun minus(m: M): M
  operator fun div(n: M): M
  operator fun times(n: M): M
  operator fun plus(m: Number): M
  operator fun minus(m: Number): M
  operator fun div(n: Number): M
  operator fun times(n: Number): M
}

interface DoubleWrapper<M: DoubleWrapper<M>>: Mathable<M>, Comparable<M> {
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

  override fun div(n: M): M {
	return fromDouble(asDouble/n.asDouble)
  }

  override fun times(n: M): M {
	return fromDouble(asDouble*n.asDouble)
  }

  override fun plus(m: Number): M {
	return fromDouble(asDouble + m.toDouble())
  }

  override fun minus(m: Number): M {
	return fromDouble(asDouble - m.toDouble())
  }

  override fun div(n: Number): M {
	return fromDouble(asDouble/n.toDouble())
  }

  override fun times(n: Number): M {
	return fromDouble(asDouble*n.toDouble())
  }

}


interface FloatWrapper<M: FloatWrapper<M>>: Mathable<M>, Comparable<M> {
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

  override fun div(n: M): M {
	return fromFloat(asFloat/n.asFloat)
  }

  override fun times(n: M): M {
	return fromFloat(asFloat*n.asFloat)
  }

  override fun plus(m: Number): M {
	return fromFloat(asFloat + m.toFloat())
  }

  override fun minus(m: Number): M {
	return fromFloat(asFloat - m.toFloat())
  }

  override fun div(n: Number): M {
	return fromFloat(asFloat/n.toFloat())
  }

  override fun times(n: Number): M {
	return fromFloat(asFloat*n.toFloat())
  }

}
