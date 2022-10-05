package matt.math.rand.poisson

import kotlin.math.exp
import kotlin.random.Random

fun Float.getPoisson(): Int {
  /*val lambda = this*/
  val L = exp(-this)
  var p = 1.0f
  var k = 0
  do {
	k++
	p *= Random.nextFloat()
  } while (p > L)
  return (k - 1)
}

/*https://stackoverflow.com/questions/1241555/algorithm-to-generate-poisson-and-binomial-random-numbers*/
fun Double.getPoisson(): Int {
  /*val lambda = this*/
  val L = exp(-this)
  var p = 1.0
  var k = 0
  do {
	k++
	p *= Random.nextDouble()
  } while (p > L)
  return (k - 1)


}