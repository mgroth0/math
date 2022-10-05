package matt.math.evenodd

import kotlin.math.abs

fun Int.isEven() = this == 0 || abs(this).mod(2) == 0
fun Int.isOdd() = !isEven()