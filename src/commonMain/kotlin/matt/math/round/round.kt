package matt.math.round

import kotlin.math.ceil
import kotlin.math.floor
import kotlin.math.roundToInt

@Suppress("unused")
fun Double.roundToDecimal(n: Int): Double {

  val temp = this*(n*10)
  val tempInt = temp.roundToInt().toDouble()
  return tempInt/(n*10)

}


fun Double.ceilInt() = ceil(this).toInt()
fun Double.floorInt() = floor(this).toInt()