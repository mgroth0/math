@file:UseSerializers(ApfloatSerializer::class, ApintSerializer::class)

package matt.math.big.point

import kotlinx.serialization.Serializable
import kotlinx.serialization.UseSerializers
import matt.math.BasicPoint
import matt.math.Point
import matt.math.big.minus
import matt.math.big.plus
import matt.math.big.ser.ApfloatSerializer
import matt.math.big.ser.ApintSerializer
import matt.math.big.sq
import matt.math.big.sqrt
import matt.math.big.toApfloat
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
