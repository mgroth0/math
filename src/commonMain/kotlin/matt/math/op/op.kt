package matt.math.op


operator fun Number.times(other: Number): Number = when (this) {
  is Short  -> when (other) {
	is Short  -> this*other
	is Int    -> this*other
	is Long   -> this*other
	is Float  -> this*other
	is Double -> this*other
	else      -> TODO()
  }

  is Int    -> when (other) {
	is Short  -> this*other
	is Int    -> this*other
	is Long   -> this*other
	is Float  -> this*other
	is Double -> this*other
	else      -> TODO()
  }

  is Long   -> when (other) {
	is Short  -> this*other
	is Int    -> this*other
	is Long   -> this*other
	is Float  -> this*other
	is Double -> this*other
	else      -> TODO()
  }

  is Float  -> when (other) {
	is Short  -> this*other
	is Int    -> this*other
	is Long   -> this*other
	is Float  -> this*other
	is Double -> this*other
	else      -> TODO()
  }

  is Double -> when (other) {
	is Short  -> this*other
	is Int    -> this*other
	is Long   -> this*other
	is Float  -> this*other
	is Double -> this*other
	else      -> TODO()
  }

  else      -> TODO()
}

operator fun Number.div(other: Number): Number = when (this) {
  is Short  -> when (other) {
	is Short  -> this/other
	is Int    -> this/other
	is Long   -> this/other
	is Float  -> this/other
	is Double -> this/other
	else      -> TODO()
  }

  is Int    -> when (other) {
	is Short  -> this/other
	is Int    -> this/other
	is Long   -> this/other
	is Float  -> this/other
	is Double -> this/other
	else      -> TODO()
  }

  is Long   -> when (other) {
	is Short  -> this/other
	is Int    -> this/other
	is Long   -> this/other
	is Float  -> this/other
	is Double -> this/other
	else      -> TODO()
  }

  is Float  -> when (other) {
	is Short  -> this/other
	is Int    -> this/other
	is Long   -> this/other
	is Float  -> this/other
	is Double -> this/other
	else      -> TODO()
  }

  is Double -> when (other) {
	is Short  -> this/other
	is Int    -> this/other
	is Long   -> this/other
	is Float  -> this/other
	is Double -> this/other
	else      -> TODO()
  }

  else      -> TODO()
}

operator fun Number.plus(other: Number): Number = when (this) {
  is Short  -> when (other) {
	is Short  -> this + other
	is Int    -> this + other
	is Long   -> this + other
	is Float  -> this + other
	is Double -> this + other
	else      -> TODO()
  }

  is Int    -> when (other) {
	is Short  -> this + other
	is Int    -> this + other
	is Long   -> this + other
	is Float  -> this + other
	is Double -> this + other
	else      -> TODO()
  }

  is Long   -> when (other) {
	is Short  -> this + other
	is Int    -> this + other
	is Long   -> this + other
	is Float  -> this + other
	is Double -> this + other
	else      -> TODO()
  }

  is Float  -> when (other) {
	is Short  -> this + other
	is Int    -> this + other
	is Long   -> this + other
	is Float  -> this + other
	is Double -> this + other
	else      -> TODO()
  }

  is Double -> when (other) {
	is Short  -> this + other
	is Int    -> this + other
	is Long   -> this + other
	is Float  -> this + other
	is Double -> this + other
	else      -> TODO()
  }

  else      -> TODO()
}

operator fun Number.minus(other: Number): Number = when (this) {
  is Short  -> when (other) {
	is Short  -> this - other
	is Int    -> this - other
	is Long   -> this - other
	is Float  -> this - other
	is Double -> this - other
	else      -> TODO()
  }

  is Int    -> when (other) {
	is Short  -> this - other
	is Int    -> this - other
	is Long   -> this - other
	is Float  -> this - other
	is Double -> this - other
	else      -> TODO()
  }

  is Long   -> when (other) {
	is Short  -> this - other
	is Int    -> this - other
	is Long   -> this - other
	is Float  -> this - other
	is Double -> this - other
	else      -> TODO()
  }

  is Float  -> when (other) {
	is Short  -> this - other
	is Int    -> this - other
	is Long   -> this - other
	is Float  -> this - other
	is Double -> this - other
	else      -> TODO()
  }

  is Double -> when (other) {
	is Short  -> this - other
	is Int    -> this - other
	is Long   -> this - other
	is Float  -> this - other
	is Double -> this - other
	else      -> TODO()
  }

  else      -> TODO()
}

operator fun Number.rem(other: Number): Number = when (this) {
  is Short  -> when (other) {
	is Short  -> this%other
	is Int    -> this%other
	is Long   -> this%other
	is Float  -> this%other
	is Double -> this%other
	else      -> TODO()
  }

  is Int    -> when (other) {
	is Short  -> this%other
	is Int    -> this%other
	is Long   -> this%other
	is Float  -> this%other
	is Double -> this%other
	else      -> TODO()
  }

  is Long   -> when (other) {
	is Short  -> this%other
	is Int    -> this%other
	is Long   -> this%other
	is Float  -> this%other
	is Double -> this%other
	else      -> TODO()
  }

  is Float  -> when (other) {
	is Short  -> this%other
	is Int    -> this%other
	is Long   -> this%other
	is Float  -> this%other
	is Double -> this%other
	else      -> TODO()
  }

  is Double -> when (other) {
	is Short  -> this%other
	is Int    -> this%other
	is Long   -> this%other
	is Float  -> this%other
	is Double -> this%other
	else      -> TODO()
  }

  else      -> TODO()
}