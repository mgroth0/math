package matt.math.geom

import kotlinx.serialization.Serializable

@Serializable
data class Geometry(
  val x: Double,
  val y: Double,
  val width: Double,
  val height: Double
)