package matt.math.reduce

import matt.math.constant.DOUBLE_ONE
import matt.math.evenodd.isOdd
import matt.model.data.mathable.Mathable
import kotlin.jvm.JvmName
import kotlin.math.ln
import kotlin.math.pow
import kotlin.math.roundToInt
import kotlin.time.Duration

fun List<Float>.mean() = sum()/size
fun FloatArray.mean() = sum()/size
fun List<Double>.mean() = sum()/size
fun Sequence<Double>.mean() = toList().mean()


fun <M: Mathable<M>> List<M>.mean() = (reduce { acc, m -> acc + m })/size
fun <M: Mathable<M>> Sequence<M>.mean() = toList().mean()


@JvmName("meanInt")
fun Sequence<Int>.mean() = map { it.toDouble() }.mean()

fun List<Double>.median() = when {
  isEmpty()    -> null
  size.isOdd() -> this[(size - 1)/2]
  else         -> (this[(size)/2] + this[(size - 1)/2])/2.0
}


fun <M: Mathable<M>> List<M>.median() = when {
  isEmpty()    -> null
  size.isOdd() -> this[(size - 1)/2]
  else         -> (this[(size)/2] + this[(size - 1)/2])/2.0
}

fun List<Duration>.median() = when {
  isEmpty()    -> null
  size.isOdd() -> this[(size - 1)/2]
  else         -> (this[(size)/2] + this[(size - 1)/2])/2.0
}


fun DoubleArray.mean() = sum()/size
fun IntArray.intMean() = (sum()/size.toDouble()).roundToInt()
fun IntArray.doubleMean() = (sum()/size.toDouble())


fun <T> Iterable<T>.meanOf(op: (T)->Double) = map { op(it) }.mean()


@OptIn(kotlin.experimental.ExperimentalTypeInference::class)
@OverloadResolutionByLambdaReturnType
@JvmName("sumOfFloat")
inline fun <T> Iterable<T>.sumOf(selector: (T)->Float): Float {
  var sum: Float = 0f
  for (element in this) {
	sum += selector(element)
  }
  return sum
}

@OptIn(kotlin.experimental.ExperimentalTypeInference::class)
@OverloadResolutionByLambdaReturnType
@JvmName("sumOfFloat")
inline fun FloatArray.sumOf(selector: (Float)->Float): Float {
  var sum: Float = 0f
  for (element in this) {
	sum += selector(element)
  }
  return sum
}

fun Sequence<Double>.geometricMean(): Double = toList().geometricMean()
fun List<Double>.geometricMean(bump: Double = 1.0) = fold(1.0) { acc, d ->
  acc*d*bump
}.pow(DOUBLE_ONE/size)


fun List<Float>.logSum() = fold(0f) { acc, d ->
  acc + ln(d)
}

fun List<Double>.logSum() = fold(0.0) { acc, d ->
  acc + ln(d)
}