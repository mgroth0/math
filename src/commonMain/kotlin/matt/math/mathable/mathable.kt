package matt.math.mathable

interface Mathable<M: Mathable<M>> {
  operator fun plus(m: M): M
  operator fun div(n: Number): M
}
