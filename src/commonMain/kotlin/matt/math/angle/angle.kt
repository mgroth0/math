package matt.math.angle

import kotlin.math.abs
import kotlin.random.Random

fun orth(degrees: Float): Float {
  require(degrees in 0.0f..180.0f)
  return if (degrees < 90.0f) degrees + 90.0f
  else degrees - 90.0f
}

fun orth(degrees: Double): Double {
  require(degrees in 0.0..180.0)
  return if (degrees < 90.0) degrees + 90.0
  else degrees - 90.0
}

fun randomAngleInDegrees() = Random.nextDouble()*360.0





/**
 * Shortest distance (angular) between two angles.
 * It will be in range [0, 180].
 */
fun angularDifference(alpha: Double, beta: Double): Double {
  val phi = abs(beta - alpha)%360.0  /*This is either the distance or 360 - distance*/
  return if (phi > 180.0) 360.0 - phi else phi
}

data class Sides(val adj: Double, val opp: Double)