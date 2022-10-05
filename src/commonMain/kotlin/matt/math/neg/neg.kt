package matt.math.neg


operator fun Number.unaryMinus(): Number {
  return when (this) {
	is Double -> -this
	is Int    -> -this
	is Long   -> -this
	is Short  -> -this
	is Float  -> -this
	else      -> throw RuntimeException("how to unary minus ${this}?")
  }
}



