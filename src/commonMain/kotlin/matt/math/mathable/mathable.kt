package matt.math.mathable

interface Mathable<M: Mathable<M>> {
  operator fun plus(m: M): M
  operator fun minus(m: M): M
  operator fun div(n: Number): M
  operator fun times(n: Number): M
}


