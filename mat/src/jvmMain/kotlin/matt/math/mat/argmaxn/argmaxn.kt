package matt.math.mat.argmaxn

import matt.lang.go
import matt.log.profile.stopwatch.tic
import org.jetbrains.kotlinx.multik.api.mk
import org.jetbrains.kotlinx.multik.api.zeros
import org.jetbrains.kotlinx.multik.ndarray.data.D1
import org.jetbrains.kotlinx.multik.ndarray.data.MultiArray
import org.jetbrains.kotlinx.multik.ndarray.data.set
import org.jetbrains.kotlinx.multik.ndarray.operations.forEachIndexed

/*Written by Matt*/
fun MultiArray<Float, D1>.argmaxn(n: Int): IntArray {
  val t = tic("argmaxn")
  require(size >= n)
  t.toc(0)
  val input = this.copy().cat(mk.zeros(0)) /*this line has horrible performance*/
  t.toc(1)
  val output = IntArray(n)
  t.toc(2)
  repeat(n) {
	val m = mk.math.argMax(input)
	output[it] = m
	input[m] = Float.MIN_VALUE
  }
  t.toc(3)

  return output

}


private val placeholder = IndexedValue(index = 0, value = 0f)

/*Written by Matt... this one is like 50 matt.math.op.times faster!!!*/
fun MultiArray<Float, D1>.argmaxn2(n: Int): List<Int> {
  require(size >= n)
  val output = MutableList(n) { placeholder }
  var outputSize = 0
  var smallestValue = 0f
  var smallestIndex = 0
  var needRefreshMin = true
  forEachIndexed { index, value ->
	if (outputSize < n) {
	  output[outputSize++] = IndexedValue(index = index, value = value)
	} else {
	  if (needRefreshMin) {
		output.withIndex().minBy { it.value.value }.go {
		  smallestValue = it.value.value
		  smallestIndex = it.index
		}
		needRefreshMin = false
	  }
	  if (value > smallestValue) {
		output[smallestIndex] = IndexedValue(index = index, value = value)
		needRefreshMin = true
	  }
	}
  }
  return output.map { it.index }

}


