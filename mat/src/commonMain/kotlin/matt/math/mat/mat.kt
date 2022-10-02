package matt.math.mat

import org.jetbrains.kotlinx.multik.api.mk
import org.jetbrains.kotlinx.multik.api.zeros
import org.jetbrains.kotlinx.multik.ndarray.data.D2
import org.jetbrains.kotlinx.multik.ndarray.data.D2Array
import org.jetbrains.kotlinx.multik.ndarray.data.NDArray
import org.jetbrains.kotlinx.multik.ndarray.data.set
import org.jetbrains.kotlinx.multik.ndarray.operations.forEachMultiIndexed
import org.jetbrains.kotlinx.multik.ndarray.operations.sum


fun <N: Number> NDArray<N, D2>.convolve(kernel: NDArray<Double, D2>): NDArray<Double, D2> {
  val result: D2Array<Double> = mk.zeros(shape[0], shape[1])
  val kPxsUsed = mutableListOf<Double>()
  val kSum = kernel.sum()

  forEachMultiIndexed { indices, px ->
	val k = mutableListOf<Double>()
	kernel.forEachMultiIndexed { kindices, kval ->
	  val kx = kindices[0] + indices[0]
	  val ky = kindices[1] + indices[1]
	  if (kx >= 0 && kx < this.shape[0] && ky >= 0 && ky < this.shape[1]) {
		k += px.toDouble()*kval
		kPxsUsed.add(kval)
	  }
	}
	result[indices[0], indices[1]] = k.sum()*(kSum/kPxsUsed.sum())
  }
  return result
}