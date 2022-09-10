@file:UseSerializers(ApfloatSerializer::class)

package matt.math.point

import kotlinx.serialization.Serializable
import kotlinx.serialization.UseSerializers
import matt.math.jmath.minus
import matt.math.jmath.plus
import matt.math.ser.ApfloatSerializer
import matt.math.jmath.sq
import matt.math.jmath.sqrt
import matt.math.jmath.toApfloat
import matt.math.BasicPoint
import matt.math.Point
import org.apfloat.Apfloat


fun BasicPoint.toAPoint() = APoint(x = x.toApfloat(), y = y.toApfloat())


@Serializable
data class APoint(
  override val x: Apfloat, override val y: Apfloat
): Point {
  fun normDist(other: APoint): Apfloat = sqrt((x - other.x).sq() + (y - other.y).sq())
  fun normDist(other: BasicPoint) = normDist(other.toAPoint())
  override val xDouble get() = x.toDouble()
  override val yDouble get() = y.toDouble()
  override fun clone(newX: Number?, newY: Number?): Point {
	return APoint(x = newX?.toApfloat() ?: x, y = newY?.toApfloat() ?: y)
  }

  override fun toBasicPoint(): BasicPoint {
	return BasicPoint(x = xDouble, y = yDouble)
  }
}



