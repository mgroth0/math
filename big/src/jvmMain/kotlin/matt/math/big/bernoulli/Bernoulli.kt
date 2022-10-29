package matt.math.big.bernoulli

import matt.math.big.rational.rational
import java.math.BigInteger
import java.util.Vector

/** Bernoulli numbers.
 * @since 2006-06-25
 * @author Richard J. Mathar
 */
class bernoulli {
  /** Set a coefficient in the internal table.
   * @param n the zero-based index of the coefficient. n=0 for the constant term.
   * @param value the new value of the coefficient.
   * @author Richard J. Mathar
   */
  protected operator fun set(n: Int, value: rational) {
	val nindx = n/2
	if (nindx < a.size) a[nindx] = value else {
	  while (a.size < nindx) a.add(rational.ZERO)
	  a.add(value)
	}
  }

  /** The Bernoulli number at the index provided.
   * @param n the index, non-negative.
   * @return the B_0=1 for n=0, B_1=-1/2 for n=1, B_2=1/6 for n=2 etc
   * @author Richard J. Mathar
   */
  fun at(n: Int): rational {
	return if (n == 1) rational(-1, 2) else if (n%2 != 0) rational.ZERO else {
	  val nindx = n/2
	  if (a.size <= nindx) {
		var i = 2*a.size
		while (i <= n) {
		  set(i, doubleSum(i))
		  i += 2
		}
	  }
	  a.elementAt(nindx)
	}
  }

  /* Generate a new B_n by a standard double sum.
     * @param n The index of the Bernoulli number.
     * @return The Bernoulli number at n.
     * @author Richard J. Mathar
     */
  private fun doubleSum(n: Int): rational {
	var resul = rational.ZERO
	for (k in 0..n) {
	  var jsum = rational.ZERO
	  var bin = BigInteger.ONE
	  for (j in 0..k) {
		val jpown = BigInteger("" + j).pow(n)
		jsum = if (j%2 == 0) jsum.add(bin.multiply(jpown)) else jsum.subtract(bin.multiply(jpown))

		/* update binomial(k,j) recursively
                 */bin = bin.multiply(BigInteger("" + (k - j))).divide(BigInteger("" + (j + 1)))
	  }
	  resul = resul.add(jsum.divide(BigInteger("" + (k + 1))))
	}
	return resul
  }

  companion object {
	/*
     * The list of all Bernoulli numbers as a vector, n=0,2,4,....
     */
	var a = Vector<rational>()
  }

  init {
	if (a.size == 0) {
	  a.add(rational.ONE)
	  a.add(rational(1, 6))
	}
  }
} /* Bernoulli */