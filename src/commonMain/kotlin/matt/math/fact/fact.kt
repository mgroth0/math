package matt.math.fact

import kotlin.math.ln

/*log (5*4*3*2*1) = log (5) + log(4) ...*/
fun Int.logFactorial(): Double {
  require(this > -1)
  if (this == 0) return 0.0
  var i = this
  var r = 0.0
  while (i > 0) {
	r += ln(i.toDouble())
	i -= 1
  }
  return r
}

fun Int.logFactorialFloat(): Float {
  require(this > -1)
  if (this == 0) return 0.0.toFloat()
  var i = this
  var r = 0.0.toFloat()
  while (i > 0) {
	r += ln(i.toFloat())
	i -= 1
  }
  return r
}