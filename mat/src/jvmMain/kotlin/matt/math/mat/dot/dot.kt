package matt.math.mat.dot

import matt.collect.itr.forEachNested
import matt.math.big.hasImag
import matt.math.big.plus
import matt.math.big.times
import matt.math.big.toApfloat
import org.apfloat.Apcomplex
import org.apfloat.Apfloat
import org.jetbrains.kotlinx.multik.ndarray.data.D2
import org.jetbrains.kotlinx.multik.ndarray.data.MultiArray
import org.jetbrains.kotlinx.multik.ndarray.data.get



infix fun MultiArray<Float, D2>.dot(other: MultiArray<Float, D2>): Float {
  require(this.shape[0] == this.shape[1] && this.shape[0] == other.shape[0] && this.shape[1] == other.shape[1])
  var ee = 0.0.toFloat()
  (0 until this.shape[0]).forEachNested { x, y ->
	val first = this[x][y]
	val second = other[x][y]
	if (!first.isNaN() && !second.isNaN()) {
	  ee += this[x][y]*other[x][y]
	}
  }
  return ee
}

infix fun MultiArray<Double, D2>.dot(other: MultiArray<Double, D2>): Double {
  require(this.shape[0] == this.shape[1] && this.shape[0] == other.shape[0] && this.shape[1] == other.shape[1])
  var ee = 0.0
  (0 until this.shape[0]).forEachNested { x, y ->
	val first = this[x][y]
	val second = other[x][y]
	if (!first.isNaN() && !second.isNaN()) {
	  ee += this[x][y]*other[x][y]
	}
  }
  return ee
}


infix fun Array<Array<Float?>>.dot(other: Array<Array<Float?>>): Float {
  require(this.size == this[0].size && this.size == other.size && this[0].size == other[0].size)
  var ee = 0.0f
  (0 until this.size).forEachNested { x, y ->
	val first = this[x][y]
	val second = other[x][y]
	if (first != null && second != null) {
	  ee += first*second
	}
  }
  return ee
}



infix fun MultiArray<Apfloat, D2>.dot(other: MultiArray<Apfloat, D2>): Apfloat {
  require(this.shape[0] == this.shape[1] && this.shape[0] == other.shape[0] && this.shape[1] == other.shape[1])
  var ee = 0.0.toApfloat()
  (0 until this.shape[0]).forEachNested { x, y ->
	ee += this[x][y]*other[x][y]
  }
  return ee

}

@JvmName("dotApcomplexD2")
infix fun MultiArray<Apcomplex, D2>.dot(other: MultiArray<Apcomplex, D2>): Apfloat {
  require(this.shape[0] == this.shape[1] && this.shape[0] == other.shape[0] && this.shape[1] == other.shape[1])
  var ee = 0.0.toApfloat()
  (0 until this.shape[0]).forEachNested { x, y ->
	val first = this[x][y]
	val second = other[x][y]
	if (!first.hasImag && !second.hasImag) {
	  ee += first.multiply(second)
	}
  }
  return ee

}