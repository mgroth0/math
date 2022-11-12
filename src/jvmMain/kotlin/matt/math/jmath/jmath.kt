package matt.math.jmath

import matt.math.angle.Sides
import matt.math.exp.sq
import org.apache.commons.math3.special.Gamma
import java.math.BigDecimal
import java.math.BigInteger
import java.math.MathContext
import kotlin.math.pow
import kotlin.math.roundToInt


val e = Math.E
const val eFloat = Math.E.toFloat()

val PI = Math.PI
val PIFloat = PI.toFloat()


fun Float.roundToDecimalPlace(n: Int): Float {
  val factor = 10.0f.pow(n)
  return (factor*this).roundToInt()/factor
}

fun Float.sigFigs(n: Int): Float {
  if (isInfinite() || isNaN()) return this
  var bd = BigDecimal(this.toDouble())
  bd = bd.round(MathContext(n))
  return bd.toFloat()
}

fun Double.sigFigs(n: Int): Double {
  if (isInfinite() || isNaN()) return this
  var bd = BigDecimal(this)
  bd = bd.round(MathContext(n))
  return bd.toDouble()
}


fun Int.simpleFactorial(): BigInteger {
  require(this > -1)
  /*println("getting simpleFact of ${this}")*/
  if (this == 0) return BigInteger.ONE
  //  if (this == 1) return 1
  return (1L..this).fold(BigInteger.ONE) { acc, i -> acc*BigInteger.valueOf(i) }
  //  var r = this*(this - 1)
  //  if ((this - 2) > 1) {
  //	((this - 2)..2).forEach { i ->
  //	  r *= i
  //	}
  //  }
  //  return r
}


fun sigmoid(x: Double): Double = 1/(1 + e.pow(-x))
fun sigmoidDerivative(x: Double): Double = e.pow(-x).let { it/(1 + it).sq() }


fun dirAndHypToAdjAndOpp(dirInDegrees: Double, hyp: Double): Sides {
  val rads = Math.toRadians(dirInDegrees)
  val opposite = kotlin.math.sin(rads)*hyp
  val adj = kotlin.math.cos(rads)*hyp
  return Sides(adj = adj, opp = opposite)
}


/*https://stackoverflow.com/questions/31539584/how-can-i-make-my-factorial-method-work-with-decimals-gamma*/
fun Double.generalizedFactorial(): Double {
  /*Gamma(n) = (n-1)! for integer n*/
  return Gamma.gamma(this + 1)
}

fun Double.generalizedFactorialOrSimpleIfInfOrNaN(): BigDecimal {
  /*Gamma(n) = (n-1)! for integer n*/
  println("getting gamma of ${this + 1}")
  return Gamma.gamma(this + 1).takeIf { !it.isInfinite() && !it.isNaN() }?.toBigDecimal() ?: roundToInt()
	.simpleFactorial()
	.toBigDecimal()
}

fun Number.sigfig(significantFigures: Int): Double {
  return BigDecimal(this.toDouble()).toSignificantFigures(significantFigures).toDouble()
}

fun BigDecimal.toSignificantFigures(significantFigures: Int): BigDecimal {
  val s = String.format("%." + significantFigures + "G", this)
  return BigDecimal(s)
}

fun Double.decimalOrScientificNotation() = String.format("%.3G", this)
