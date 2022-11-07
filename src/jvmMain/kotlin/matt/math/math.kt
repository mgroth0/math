@file:JvmName("MathJvmKt")

package matt.math

/*Written by Matt*/
fun FloatArray.argmaxn(n: Int): IntArray {
  //  val t= tic("matt.math.argmaxn")
  //  val input = this.copy().cat(mk.zeros(0)) /*this line has horrible performance*/
  val input = FloatArray(size)
  copyInto(input)
  //  t.toc(1)
  val output = IntArray(n)
  //  t.toc(2)
  repeat(n) {
	val m = withIndex().maxBy { it.value }.index
	//	val m = mk.math.argMax(input)
	output[it] = m
	input[m] = Float.MIN_VALUE
  }
  //  t.toc(3)
  return output
}

fun degreesToRadians(degrees: Double) = degrees*kotlin.math.PI/180