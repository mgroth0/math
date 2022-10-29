@file:Suppress("NAME_SHADOWING", "unused", "UNUSED_VARIABLE", "UNCHECKED_CAST", "CovariantEquals",
  "KDocUnresolvedReference", "SpellCheckingInspection", "PARAMETER_NAME_CHANGED_ON_OVERRIDE"
)

package matt.math.big.ifactor

import matt.math.big.prime.prime
import matt.math.big.rational.rational
import java.math.BigInteger
import java.util.Collections
import java.util.Vector

/**
 * Factored integers.
 * This class contains a non-negative integer with the prime factor decomposition attached.
 *
 * @author Richard J. Mathar
 * @since 2006-08-14
 * @since 2012-02-14 The internal representation contains the bases, and becomes sparser if few
 * prime factors are present.
 */
class Ifactor: Cloneable, Comparable<Ifactor> {
  /**
   * The standard representation of the number
   */
  var n: BigInteger? = null

  /*
     * The bases and powers of the prime factorization.
     * representation n = primeexp[0]^primeexp[1]*primeexp[2]^primeexp[3]*...
     * The value 0 is represented by an empty vector, the value 1 by a vector of length 1
     * with a single power of 0.
     */
  var primeexp: Vector<Int>

  /**
   * Constructor given an integer.
   * constructor with an ordinary integer
   *
   * @param number the standard representation of the integer
   * @author Richard J. Mathar
   */
  constructor(number: Int) {
	var number = number
	n = BigInteger("" + number)
	primeexp = Vector()
	if (number > 1) {
	  var primindx = 0
	  val primes = prime()
	  /* Test division against all primes.
             */while (number > 1) {
		var ex = 0
		/* primindx=0 refers to 2, =1 to 3, =2 to 5, =3 to 7 etc
                 */
		val p = primes.at(primindx).toInt()
		while (number%p == 0) {
		  ex++
		  number /= p
		  if (number == 1) break
		}
		if (ex > 0) {
		  primeexp.add(p)
		  primeexp.add(ex)
		}
		primindx++
	  }
	} else if (number == 1) {
	  primeexp.add(1)
	  primeexp.add(0)
	}
  } /* Ifactor */

  /**
   * Constructor given a BigInteger .
   * Constructor with an ordinary integer, calling a prime factor decomposition.
   *
   * @param number the BigInteger representation of the integer
   * @author Richard J. Mathar
   */
  constructor(number: BigInteger) {
	var number = number
	n = number
	primeexp = Vector()
	if (number.compareTo(BigInteger.ONE) == 0) {
	  primeexp.add(1)
	  primeexp.add(0)
	} else {
	  var primindx = 0
	  val primes = prime()
	  /* Test for division against all primes.
             */while (number.compareTo(BigInteger.ONE) == 1) {
		var ex = 0
		val p = primes.at(primindx)
		while (number.remainder(p).compareTo(BigInteger.ZERO) == 0) {
		  ex++
		  number = number.divide(p)
		  if (number.compareTo(BigInteger.ONE) == 0) break
		}
		if (ex > 0) {
		  primeexp.add(p.toInt())
		  primeexp.add(ex)
		}
		primindx++
	  }
	}
  } /* Ifactor */

  /**
   * Constructor given a list of exponents of the prime factor decomposition.
   *
   * @param pows the vector with the sorted list of exponents.
   * pows[0] is the exponent of 2, pows[1] the exponent of 3, pows[2] the exponent of 5 etc.
   * Note that this list does not include the primes, but assumes a continuous prime-smooth basis.
   * @author Richard J. Mathar
   */
  constructor(pows: Vector<Int>) {
	primeexp = Vector(2*pows.size)
	if (pows.size > 0) {
	  n = BigInteger.ONE
	  val primes = prime()
	  /* Build the full number by the product of all powers of the primes.
             */for (primindx in pows.indices) {
		val ex = pows.elementAt(primindx).toInt()
		val p = primes.at(primindx)
		n = n!!.multiply(p.pow(ex))
		primeexp.add(p.toInt())
		primeexp.add(ex)
	  }
	} else n = BigInteger.ZERO
  } /* Ifactor */

  /**
   * Copy constructor.
   *
   * @param oth the value to be copied
   * @author Richard J. Mathar
   */
  constructor(oth: Ifactor) {
	n = oth.n
	primeexp = oth.primeexp
  } /* Ifactor */

  /**
   * Deep copy.
   *
   * @author Richard J. Mathar
   * @since 2009-08-14
   */
  public override fun clone(): Ifactor {
	val p = primeexp.clone() as Vector<Int>
	val cl = Ifactor(0)
	cl.n = BigInteger("" + n)
	return cl
  } /* Ifactor.clone */

  /**
   * Comparison of two numbers.
   * The value of this method is in allowing the Vector.contains() calls that use the value,
   * not the reference for comparison.
   *
   * @param oth the number to compare this with.
   * @return true if both are the same numbers, false otherwise.
   * @author Richard J. Mathar
   */
  fun equals(oth: Ifactor): Boolean {
	return n!!.compareTo(oth.n) == 0
  } /* Ifactor.equals */

  /**
   * Multiply with another positive integer.
   *
   * @param oth the second factor.
   * @return the product of both numbers.
   * @author Richard J. Mathar
   */
  fun multiply(oth: BigInteger): Ifactor {
	/* the optimization is to factorize oth _before_ multiplying
         */
	return multiply(Ifactor(oth))
  } /* Ifactor.multiply */

  /**
   * Multiply with another positive integer.
   *
   * @param oth the second factor.
   * @return the product of both numbers.
   * @author Richard J. Mathar
   */
  fun multiply(oth: Int): Ifactor {
	/* the optimization is to factorize oth _before_ multiplying
         */
	return multiply(Ifactor(oth))
  } /* Ifactor.multiply */

  /**
   * Multiply with another positive integer.
   *
   * @param oth the second factor.
   * @return the product of both numbers.
   * @author Richard J. Mathar
   */
  fun multiply(oth: Ifactor): Ifactor {
	/* This might be done similar to the lcm() implementation by adding
         * the powers of the components and calling the constructor with the
         * list of exponents. This here is the simplest implementation, but slow because
         * it calls another prime factorization of the product:
         * return( new Ifactor(n.multiply(oth.n))) ;
         */
	return multGcdLcm(oth, 0)
  }

  /**
   * Lowest common multiple of this with oth.
   *
   * @param oth the second parameter of lcm(this,oth)
   * @return the lowest common multiple of both numbers. Returns zero
   * if any of both arguments is zero.
   * @author Richard J. Mathar
   */
  fun lcm(oth: Ifactor): Ifactor {
	return multGcdLcm(oth, 2)
  }

  /**
   * Greatest common divisor of this and oth.
   *
   * @param oth the second parameter of gcd(this,oth)
   * @return the lowest common multiple of both numbers. Returns zero
   * if any of both arguments is zero.
   * @author Richard J. Mathar
   */
  fun gcd(oth: Ifactor): Ifactor {
	return multGcdLcm(oth, 1)
  }

  /**
   * Multiply with another positive integer.
   *
   * @param oth  the second factor.
   * @param type 0 to multiply, 1 for gcd, 2 for lcm
   * @return the product, gcd or lcm of both numbers.
   * @author Richard J. Mathar
   */
  protected fun multGcdLcm(oth: Ifactor, type: Int): Ifactor {
	val prod = Ifactor(0)
	/* skip the case where 0*something =0, falling thru to the empty representation for 0
         */if (primeexp.size != 0 && oth.primeexp.size != 0) {
	  /* Cases of 1 matt.math.op.times something return something.
             * Cases of lcm(1, something) return something.
             * Cases of gcd(1, something) return 1.
             */
	  if (primeexp.firstElement().toInt() == 1 && type == 0) return oth else if (primeexp.firstElement()
																					 .toInt() == 1 && type == 2) return oth else if (primeexp.firstElement()
																																		 .toInt() == 1 && type == 1) return this else if (oth.primeexp.firstElement()
																																															  .toInt() == 1 && type == 0) return this else if (oth.primeexp.firstElement()
																																																												   .toInt() == 1 && type == 2) return this else if (oth.primeexp.firstElement()
																																																																										.toInt() == 1 && type == 1) return oth else {
		var idxThis = 0
		var idxOth = 0
		when (type) {
		  0 -> prod.n = n!!.multiply(oth.n)
		  1 -> prod.n = n!!.gcd(oth.n)
		  2 ->                         /* the awkward way, lcm = product divided by gcd
                         */prod.n = n!!.multiply(oth.n).divide(n!!.gcd(oth.n))
		}

		/* scan both representations left to right, increasing prime powers
                 */while (idxOth < oth.primeexp.size || idxThis < primeexp.size) {
		  if (idxOth >= oth.primeexp.size) {
			/* exhausted the list in oth.primeexp; copy over the remaining 'this'
                         * if multiplying or lcm, discard if gcd.
                         */
			if (type == 0 || type == 2) {
			  prod.primeexp.add(primeexp.elementAt(idxThis))
			  prod.primeexp.add(primeexp.elementAt(idxThis + 1))
			}
			idxThis += 2
		  } else if (idxThis >= primeexp.size) {
			/* exhausted the list in primeexp; copy over the remaining 'oth'
                         */
			if (type == 0 || type == 2) {
			  prod.primeexp.add(oth.primeexp.elementAt(idxOth))
			  prod.primeexp.add(oth.primeexp.elementAt(idxOth + 1))
			}
			idxOth += 2
		  } else {
			var p: Int
			var ex: Int
			when (primeexp.elementAt(idxThis).compareTo(oth.primeexp.elementAt(idxOth))) {
			  0    -> {
				/* same prime bases p in both factors */p = primeexp.elementAt(idxThis)
				ex = when (type) {
				  0 ->                                         /* product means adding exponents */primeexp.elementAt(
					idxThis + 1
				  )
																									   .toInt() +
																								   oth.primeexp.elementAt(
																									 idxOth + 1
																								   )
																									   .toInt()
				  1 ->                                         /* gcd means minimum of exponents */Math.min(
					primeexp.elementAt(idxThis + 1).toInt(),
					oth.primeexp.elementAt(idxOth + 1).toInt()
				  )
				  else ->                                         /* lcm means maximum of exponents */Math.max(
					primeexp.elementAt(idxThis + 1).toInt(),
					oth.primeexp.elementAt(idxOth + 1).toInt()
				  )
				}
				prod.primeexp.add(p)
				prod.primeexp.add(ex)
				idxOth += 2
				idxThis += 2
			  }
			  1    -> {
				/* this prime base bigger than the other and taken later */if (type == 0 || type == 2) {
				  prod.primeexp.add(oth.primeexp.elementAt(idxOth))
				  prod.primeexp.add(oth.primeexp.elementAt(idxOth + 1))
				}
				idxOth += 2
			  }
			  else -> {
				/* this prime base smaller than the other and taken now */if (type == 0 || type == 2) {
				  prod.primeexp.add(primeexp.elementAt(idxThis))
				  prod.primeexp.add(primeexp.elementAt(idxThis + 1))
				}
				idxThis += 2
			  }
			}
		  }
		}
	  }
	}
	return prod
  } /* Ifactor.multGcdLcm */

  /**
   * Integer division through  another positive integer.
   *
   * @param oth the denominator.
   * @return the division of this through the oth, discarding the remainder.
   * @author Richard J. Mathar
   */
  fun divide(oth: Ifactor): Ifactor {
	/* todo: it'd probably be faster to cancel the gcd(this,oth) first in the prime power
         * representation, which would avoid a more strenous factorization of the integer ratio
         */
	return Ifactor(n!!.divide(oth.n))
  } /* Ifactor.divide */

  /**
   * Summation with another positive integer
   *
   * @param oth the other term.
   * @return the sum of both numbers
   * @author Richard J. Mathar
   */
  fun add(oth: BigInteger): Ifactor {
	/* avoid refactorization if oth is zero...
         */
	return if (oth.compareTo(BigInteger.ZERO) != 0) Ifactor(n!!.add(oth)) else this
  } /* Ifactor.add */

  /**
   * Exponentiation with a positive integer.
   *
   * @param exponent the non-negative exponent
   * @return n^exponent. If exponent=0, the result is 1.
   * @author Richard J. Mathar
   */
  @Throws(ArithmeticException::class)
  fun pow(exponent: Int): Ifactor {
	/* three simple cases first
         */
	if (exponent < 0) throw ArithmeticException("Cannot raise " + toString() + " to negative " + exponent) else if (exponent == 0) return Ifactor(
	  1
	) else if (exponent == 1) return this

	/* general case, the vector with the prime factor powers, which are component-wise
         * exponentiation of the individual prime factor powers.
         */
	val pows = Ifactor(0)
	var i = 0
	while (i < primeexp.size) {
	  val p = primeexp.elementAt(i)
	  val ex = primeexp.elementAt(i + 1).toInt()
	  pows.primeexp.add(p)
	  pows.primeexp.add(ex*exponent)
	  i += 2
	}
	return pows
  } /* Ifactor.pow */

  /**
   * Pulling the r-th root.
   *
   * @param r the positive or negative (nonzero) root.
   * @return n^(1/r).
   * The return value falls into the Ifactor class if r is positive, but if r is negative
   * a Rational type is needed.
   * @author Richard J. Mathar
   * @since 2009-05-18
   */
  @Throws(ArithmeticException::class)
  fun root(r: Int): rational {
	return if (r == 0) throw ArithmeticException("Cannot pull zeroth root of " + toString()) else if (r < 0) {
	  /* a^(-1/b)= 1/(a^(1/b))
             */
	  val invRoot = root(-r)
	  rational.ONE.divide(invRoot)
	} else {
	  val pows = BigInteger.ONE
	  var i = 0
	  while (i < primeexp.size) {

		/* all exponents must be multiples of r to succeed (that is, to
                 * stay in the range of rational results).
                 */
		val ex = primeexp.elementAt(i + 1).toInt()
		if (ex%r != 0) throw ArithmeticException("Cannot pull " + r + "th root of " + toString())
		pows.multiply(BigInteger("" + primeexp.elementAt(i)).pow(ex/r))
		i += 2
	  }
	  /* convert result to a Rational; unfortunately this will loose the prime factorization */rational(pows)
	}
  } /* Ifactor.root */

  /**
   * The set of positive divisors.
   *
   * @return the vector of divisors of the absolute value, sorted.
   * @author Richard J. Mathar
   * @since 2010-08-27
   */
  fun divisors(): Vector<BigInteger> {
	/* Recursive approach: the divisors of p1^e1*p2^e2*..*py^ey*pz^ez are
         * the divisors that don't contain  the factor pz, and the
         * divisors that contain any power of pz between 1 and up to ez multiplied
         * by 1 or by a product that contains the factors p1..py.
         */
	val d = Vector<BigInteger>()
	if (n!!.compareTo(BigInteger.ZERO) == 0) return d
	d.add(BigInteger.ONE)
	if (n!!.compareTo(BigInteger.ONE) > 0) {
	  /* Computes sigmaIncopml(p1^e*p2^e2...*py^ey) */
	  val dp = dropPrime()

	  /* get ez */
	  val ez = primeexp.lastElement().toInt()
	  val partd = dp.divisors()

	  /* obtain pz by lookup in the prime list */
	  val pz = BigInteger(primeexp.elementAt(primeexp.size - 2).toString())

	  /* the output contains all products of the form partd[]*pz^ez, ez>0,
             * and with the exception of the 1, all these are appended.
             */for (i in 1 until partd.size) d.add(partd.elementAt(i))
	  for (e in 1..ez) {
		val pzez = pz.pow(e)
		for (i in partd.indices) d.add(partd.elementAt(i).multiply(pzez))
	  }
	}
	Collections.sort(d)
	return d
  } /* Ifactor.divisors */
  /**
   * Sum of the k-th powers of divisors of the number.
   *
   * @param k The exponent of the powers.
   * @return the sum of all divisors of the number, 1^k+....+n^k.
   * @author Richard J. Mathar
   */
  /**
   * Sum of the divisors of the number.
   *
   * @return the sum of all divisors of the number, 1+....+n.
   * @author Richard J. Mathar
   */
  @JvmOverloads
  fun sigma(k: Int = 1): Ifactor {
	/* the question is whether keeping a factorization  is worth the effort
         * or whether one should simply multiply these to return a BigInteger...
         */
	return if (n!!.compareTo(BigInteger.ONE) == 0) ONE else if (n!!.compareTo(BigInteger.ZERO) == 0) ZERO else {
	  /* multiplicative: sigma_k(p^e) = [p^(k*(e+1))-1]/[p^k-1]
             * sigma_0(p^e) = e+1.
             */
	  var resul = ONE
	  var i = 0
	  while (i < primeexp.size) {
		val ex = primeexp.elementAt(i + 1).toInt()
		resul = if (k == 0) resul.multiply(ex + 1) else {
		  val p = primeexp.elementAt(i)
		  val num = BigInteger(p.toString()).pow(k*(ex + 1)).subtract(BigInteger.ONE)
		  val deno = BigInteger(p.toString()).pow(k).subtract(BigInteger.ONE)
		  /* This division is of course exact, no remainder
                     * The costly prime factorization is hidden here.
                     */
		  val f = Ifactor(num.divide(deno))
		  resul.multiply(f)
		}
		i += 2
	  }
	  resul
	}
  } /* Ifactor.sigma */ /* Ifactor.sigma */

  /**
   * Divide through the highest possible power of the highest prime.
   * If the current number is the prime factor product p1^e1 * p2*e2* p3^e3*...*py^ey * pz^ez,
   * the value returned has the final factor pz^ez eliminated, which gives
   * p1^e1 * p2*e2* p3^e3*...*py^ey.
   *
   * @return the new integer obtained by removing the highest prime power.
   * If this here represents 0 or 1, it is returned without change.
   * @author Richard J. Mathar
   * @since 2006-08-20
   */
  fun dropPrime(): Ifactor {
	/* the cases n==1 or n ==0
         */
	if (n!!.compareTo(BigInteger.ONE) <= 0) return this

	/* The cases n>1
         * Start empty. Copy all but the last factor over to the result
         * the vector with the new prime factor powers, which contain the
         * old prime factor powers up to but not including the last one.
         */
	val pows = Ifactor(0)
	pows.n = BigInteger.ONE
	var i = 0
	while (i < primeexp.size - 2) {
	  pows.primeexp.add(primeexp.elementAt(i))
	  pows.primeexp.add(primeexp.elementAt(i + 1))
	  val p = BigInteger(primeexp.elementAt(i).toString())
	  val ex = primeexp.elementAt(i + 1).toInt()
	  pows.n = pows.n!!.multiply(p.pow(ex))
	  i += 2
	}
	return pows
  } /* Ifactor.dropPrime */

  /**
   * Test whether this is a square of an integer (perfect square).
   *
   * @return true if this is an integer squared (including 0), else false
   * @author Richard J. Mathar
   */
  fun issquare(): Boolean {
	val resul = true
	/* check the exponents, located at the odd-indexed positions
         */
	var i = 1
	while (i < primeexp.size) {
	  if (primeexp.elementAt(i).toInt()%2 != 0) return false
	  i += 2
	}
	return true
  } /* Ifactor.issquare */

  /**
   * The sum of the prime factor exponents, with multiplicity.
   *
   * @return the sum over the primeexp numbers
   * @author Richard J. Mathar
   */
  fun bigomega(): Int {
	var resul = 0
	var i = 1
	while (i < primeexp.size) {
	  resul += primeexp.elementAt(i).toInt()
	  i += 2
	}
	return resul
  } /* Ifactor.bigomega */

  /**
   * The sum of the prime factor exponents, without multiplicity.
   *
   * @return the number of distinct prime factors.
   * @author Richard J. Mathar
   * @since 2008-10-16
   */
  fun omega(): Int {
	return primeexp.size/2
  } /* Ifactor.omega */

  /**
   * The square-free part.
   *
   * @return the minimum m such that m matt.math.op.times this number is a square.
   * @author Richard J. Mathar
   * @since 2008-10-16
   */
  fun core(): BigInteger {
	var resul = BigInteger.ONE
	var i = 0
	while (i < primeexp.size) {
	  if (primeexp.elementAt(i + 1).toInt()%2 != 0) resul = resul.multiply(BigInteger(primeexp.elementAt(i).toString()))
	  i += 2
	}
	return resul
  } /* Ifactor.core */

  /**
   * The Moebius function.
   * 1 if n=1, else, if k is the number of distinct prime factors, return (-1)^k,
   * else, if k has repeated prime factors, return 0.
   *
   * @return the moebius function.
   * @author Richard J. Mathar
   */
  fun moebius(): Int {
	if (n!!.compareTo(BigInteger.ONE) <= 0) return 1
	/* accumulate number of different primes in k */
	var k = 1
	var i = 0
	while (i < primeexp.size) {
	  val e = primeexp.elementAt(i + 1).toInt()
	  if (e > 1) return 0 else if (e == 1) /* accumulates (-1)^k */ k *= -1
	  i += 2
	}
	return k
  } /* Ifactor.moebius */

  /**
   * Maximum of two values.
   *
   * @param oth the number to compare this with.
   * @return the larger of the two values.
   * @author Richard J. Mathar
   */
  fun max(oth: Ifactor): Ifactor {
	return if (n!!.compareTo(oth.n) >= 0) this else oth
  } /* Ifactor.max */

  /**
   * Minimum of two values.
   *
   * @param oth the number to compare this with.
   * @return the smaller of the two values.
   * @author Richard J. Mathar
   */
  fun min(oth: Ifactor): Ifactor {
	return if (n!!.compareTo(oth.n) <= 0) this else oth
  } /* Ifactor.min */

  /**
   * Compare value against another Ifactor
   *
   * @param oth The value to be compared agains.
   * @return 1, 0 or -1 according to being larger, equal to or smaller than oth.
   * @author Richard J. Mathar
   * @since 2012-02-15
   */
  override fun compareTo(oth: Ifactor): Int {
	return n!!.compareTo(oth.n)
  } /* compareTo */

  /**
   * Convert to printable format
   *
   * @return a string of the form n:prime^pow*prime^pow*prime^pow...
   * @author Richard J. Mathar
   */
  override fun toString(): String {
	var resul = "$n:"
	if (n!!.compareTo(BigInteger.ONE) == 0) resul += "1" else {
	  var firstMul = true
	  var i = 0
	  while (i < primeexp.size) {
		if (!firstMul) resul += "*"
		resul += if (primeexp.elementAt(i + 1).toInt() > 1) primeexp.elementAt(i).toString() + "^" + primeexp.elementAt(
		  i + 1
		)
			.toString() else primeexp.elementAt(i).toString()
		firstMul = false
		i += 2
	  }
	}
	return resul
  } /* Ifactor.toString */

  companion object {
	val ONE = Ifactor(1)
	val ZERO = Ifactor(0)

	/**
	 * Maximum of a list of values.
	 *
	 * @param set list of numbers.
	 * @return the largest in the list.
	 * @author Richard J. Mathar
	 */
	fun max(set: Vector<Ifactor>): Ifactor {
	  var resul = set.elementAt(0)
	  for (i in 1 until set.size) resul = resul.max(set.elementAt(i))
	  return resul
	} /* Ifactor.max */

	/**
	 * Minimum of a list of values.
	 *
	 * @param set list of numbers.
	 * @return the smallest in the list.
	 * @author Richard J. Mathar
	 */
	fun min(set: Vector<Ifactor>): Ifactor {
	  var resul = set.elementAt(0)
	  for (i in 1 until set.size) resul = resul.min(set.elementAt(i))
	  return resul
	} /* Ifactor.min */

	/**
	 * Test program.
	 * It takes a single argument n and prints the integer factorizaton.<br></br>
	 * java -cp . org.nevec.rjm.Ifactor n<br></br>
	 *
	 * @param args It takes a single argument n and prints the integer factorizaton.<br></br>
	 * @author Richard J. Mathar
	 */
	@JvmStatic
	fun main(args: Array<String>) {
	  val n = BigInteger(args[0])
	  println(Ifactor(n))
	} /* Ifactor.main */
  }
} /* Ifactor */