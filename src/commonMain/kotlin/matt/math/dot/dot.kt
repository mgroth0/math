package matt.math.dot

infix fun FloatArray.dot(other: FloatArray): Float {
  require(this.size == other.size)
  var ee = 0.0.toFloat()
  (0 until this.size).forEach { x ->
	val first = this[x]
	val second = other[x]
	if (!first.isNaN() && !second.isNaN()) {
	  val r = this[x]*other[x]
	  ee += r
	}
  }
  return ee
}

infix fun DoubleArray.dot(other: DoubleArray): Double {
  require(this.size == other.size)
  var ee = 0.0
  (0 until this.size).forEach { x ->
	val first = this[x]
	val second = other[x]
	if (!first.isNaN() && !second.isNaN()) {
	  val r = this[x]*other[x]
	  ee += r
	}
  }
  return ee

}

infix fun Array<out Float?>.dot(other: Array<out Float?>): Float {
  require(this.size == other.size)
  var ee = 0.0f
  (0 until this.size).forEach { x ->
	val first = this[x]
	val second = other[x]
	if (first != null && second != null) {
	  val r = first*second
	  ee += r
	}
  }
  return ee

}


