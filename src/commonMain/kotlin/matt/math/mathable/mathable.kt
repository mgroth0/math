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

