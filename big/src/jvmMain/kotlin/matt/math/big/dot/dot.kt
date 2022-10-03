package matt.math.big.dot

import matt.math.big.plus
import matt.math.big.times
import matt.math.big.toApfloat
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
