package matt.math.big.dot

import matt.collect.itr.forEachNested
import matt.math.big.hasImag
import matt.math.big.plus
import matt.math.big.times
import matt.math.big.toApfloat
import org.apfloat.Apcomplex
import org.apfloat.Apfloat

infix fun Array<out Apfloat?>.dotA(other: Array<out Apfloat?>): Apfloat {
  require(this.size == other.size)
  var ee = 0.0.toApfloat()
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



infix fun Array<Array<Apfloat?>>.dot(other: Array<Array<Apfloat?>>): Apfloat {
  require(this.size == this[0].size && this.size == other.size && this[0].size == other[0].size)
  var ee = 0.0.toApfloat()
  (0 until this.size).forEachNested { x, y ->
	val first = this[x][y]
	val second = other[x][y]
	if (first != null && second != null) {
	  ee += first*second
	}
  }
  return ee
}

infix fun Array<Array<Double?>>.dot(other: Array<Array<Double?>>): Apfloat {
  require(this.size == this[0].size && this.size == other.size && this[0].size == other[0].size)
  var ee = 0.0.toApfloat()
  (0 until this.size).forEachNested { x, y ->
	val first = this[x][y]
	val second = other[x][y]
	if (first != null && second != null) {
	  ee += first*second
	}
  }
  return ee
}


infix fun Array<Apcomplex>.dot(other: Array<Apcomplex>): Apfloat {
  require(this.size == other.size)
  var ee = 0.0.toApfloat()
  (0 until this.size).forEach { x ->
	val first = this[x]
	val second = other[x]
	if (!first.hasImag && !second.hasImag) {
	  val r = this[x].multiply(other[x])
	  ee += r
	}
  }
  return ee

}

infix fun Array<Apfloat>.dot(other: Array<Apfloat>): Apfloat {
  require(this.size == other.size)
  var ee = 0.0.toApfloat()
  (0 until this.size).forEach { x ->
	val first = this[x]
	val second = other[x]
	if (!first.hasImag && !second.hasImag) {
	  val r = this[x]*other[x]
	  ee += r
	}
  }
  return ee
}