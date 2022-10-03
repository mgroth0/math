package matt.math.big.ranges

import matt.math.big.assertRound
import matt.math.big.minus
import matt.math.big.plus
import matt.math.big.toApfloat
import org.apfloat.Apcomplex
import org.apfloat.Apfloat
import org.apfloat.ApfloatMath
import org.apfloat.Apint
import java.math.RoundingMode.UNNECESSARY


infix fun ClosedRange<Apfloat>.step(step: Apfloat): Iterable<Apfloat> {
  /*require(start != Apfloat)*/
  /*require(endInclusive.isFinite())*/
  require(step > 0.0.toApfloat()) { "Step must be positive, was: $step." }
  val sequence = generateSequence(start) { previous ->
	/*if (previous == Double.POSITIVE_INFINITY) return@generateSequence null*/
	val next = previous + step
	if (next > endInclusive) null else next
  }
  return sequence.asIterable()
}

infix fun ClosedRange<Apint>.step(step: Apint): Iterable<Apint> {
  /*require(start != Apfloat)*/
  /*require(endInclusive.isFinite())*/
  require(step > Apint.ZERO) { "Step must be positive, was: $step." }
  val sequence = generateSequence(start) { previous ->
	/*if (previous == Double.POSITIVE_INFINITY) return@generateSequence null*/
	val next = previous + step
	if (next > endInclusive) null else ApfloatMath.round(next, 20, UNNECESSARY).truncate()
  }
  return sequence.asIterable()
}

infix fun Apint.until(to: Apint): Iterable<Apint> {
  if (to <= Int.MIN_VALUE) return listOf()
  return this..((to - 1).assertRound()) step Apcomplex.ONE
}
