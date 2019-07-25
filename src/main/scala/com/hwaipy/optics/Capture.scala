package com.hwaipy.optics

import java.awt.Color
import java.awt.image.BufferedImage
import java.util.concurrent.Executors

import breeze.math.Complex
import breeze.linalg.linspace
import org.platanios.tensorflow.api.Tensor

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random

class Capture(val f: Float, val z: Float, val size: Float = 1, val grid: Int = 200, val n: Float = 1.5f, val lambda: Float = 1.55e-3f, val d: Float = 3f)(implicit val ec: ExecutionContext) {
  val R = f * (n - 1)
  val k = (2 * math.Pi / lambda).toFloat

  def testTensorFlow() = {
    val random = new Random()
    val data = Range(0, 1000000).toArray.map(_ => random.nextDouble())
    Util.tic("Test TensorFlow")
    data.map(d => d * d)
    Util.tic("without TensorFlow")
    val dT = Tensor[Double](data)
    dT * dT
    Util.tic("with TensorFlow")
    dT.gpu()
  }

  def captureAmplitudeWithRoundedWavefront(wavefront: Wavefront) = {
    val roundedWavefront = roundWavefront(wavefront)
    val pixels = new Array[Array[Complex]](grid)
    Range(0, grid).map(i => pixels(i) = Array.fill(grid)(Complex.zero))
    val captureXs = linspace(-size / 2, size / 2, grid).data.map(_.toFloat)
    val captureYs = linspace(-size / 2, size / 2, grid).data.map(_.toFloat)
    val wavefrontXs = Tensor[Float](roundedWavefront.map(_.x.toFloat))
    val wavefrontYs = Tensor[Float](roundedWavefront.map(_.y.toFloat))
    val wavefrontEs = roundedWavefront.map(_.E)
    val wavefrontPhaseLens = roundedWavefront.map(_.phaseLens.toFloat)
    val wavefrontMILERs = roundedWavefront.map(_.miLE.real.toFloat)
    val wavefrontMILEIs = roundedWavefront.map(_.miLE.imag.toFloat)
    val z2 = z * z
    Util.tic("ready for loop4")
    val workRrid = 5
    for (captureXI <- 0 until workRrid) {
      val captureX = captureXs(captureXI)
      for (captureYI <- 0 until workRrid) {
        val captureY = captureYs(captureYI)

        //        val deltaXs = Array.fill(wavefrontXs.size)(captureX).zip(wavefrontXs).map(z => z._1 - z._2)
        //        val deltaYs = Array.fill(wavefrontYs.size)(captureY).zip(wavefrontYs).map(z => z._1 - z._2)
        //        val rs = deltaXs.zip(deltaYs).map(zip => math.sqrt(zip._1 * zip._1 + zip._2 * zip._2 + z2).toFloat)
        //        val phaseLenses = wavefrontPhaseLens.toArray
        //        val phases = phaseLenses.zip(rs).map(zip => ((zip._1 + zip._2) * k) % (2 * math.Pi.toFloat))
        //        val c1Rs = phases.map(p => math.cos(p).toFloat)
        //        val c1Is = phases.map(p => math.sin(p).toFloat)
        //        val IT3 = rs.map(r => (1 + z / r) / r)
        //        val c2Rs = IT3.zip(wavefrontMILERs).map(zip => zip._1 * zip._2.toFloat)
        //        val c2Is = IT3.zip(wavefrontMILEIs).map(zip => zip._1 * zip._2.toFloat)
        //        val tRRs = c1Rs.zip(c2Rs).map(z => z._1 * z._2)
        //        val tIIs = c1Is.zip(c2Is).map(z => z._1 * z._2)
        //        val tRIs = c1Rs.zip(c2Is).map(z => z._1 * z._2)
        //        val tIRs = c1Is.zip(c2Rs).map(z => z._1 * z._2)
        //        val rRs = tRRs.zip(tIIs).map(z => z._1 - z._2)
        //        val rIs = tRIs.zip(tIRs).map(z => z._1 + z._2)
        //        pixels(captureYI)(captureXI) = Complex(rRs.sum, rIs.sum)

        val deltaXs = wavefrontXs - captureX
        val deltaYs = wavefrontYs - captureY
        val rs = (deltaXs.square + deltaXs.square + z2).sqrt
        val phaseLenses = Tensor[Float](wavefrontPhaseLens)
        val phases = ((phaseLenses + rs) * k) % (2 * math.Pi.toFloat)
        val c1Rs = phases.cos
        val c1Is = phases.sin
        val IT3 = (1 + z / rs) / rs
        val c2Rs = IT3 * Tensor[Float](wavefrontMILERs)
        val c2Is = IT3 * Tensor[Float](wavefrontMILEIs)
        val tRRs = c1Rs * c2Rs
        val tIIs = c1Is * c2Is
        val tRIs = c1Rs * c2Is
        val tIRs = c1Is * c2Rs
        val rRs = tRRs - tIIs
        val rIs = tRIs + tIRs
        pixels(captureYI)(captureXI) = Complex(rRs.entriesIterator.toList.sum, rIs.entriesIterator.toList.sum)
      }
    }
    pixels
  }

  //  def captureAmplitudeWithRoundedWavefront(wavefront: Wavefront) = {
  //    val roundedWavefront = roundWavefront(wavefront)
  //    val pixels = new Array[Array[Complex]](grid)
  //    Range(0, grid).map(i => pixels(i) = Array.fill(grid)(Complex.zero))
  //    val captureXs = linspace(-size / 2, size / 2, grid).data
  //    val captureYs = linspace(-size / 2, size / 2, grid).data
  //    Util.tic("ready for loop4")
  //    val futures = new ListBuffer[Future[Unit]]()
  //    for (captureXI <- 0 until grid) {
  //      val captureX = captureXs(captureXI)
  //      for (captureYI <- 0 until grid) {
  //        val captureY = captureYs(captureYI)
  //        val future = Future[Unit] {
  //          val its = roundedWavefront.map(rw => {
  //            val wavefrontX = rw.x
  //            val wavefrontY = rw.y
  //            val deltaX = captureX - wavefrontX
  //            val deltaY = captureY - wavefrontY
  //            val r = math.sqrt(deltaX * deltaX + deltaY * deltaY + z * z)
  //            val phaseLens = rw.phaseLens
  //            val phase = ((phaseLens + r) * k) % (2 * math.Pi)
  //            val cosP = math.cos(phase)
  //            val sinP = math.sqrt(1 - cosP * cosP) * (if ((phase > 0 && phase < math.Pi) || (phase < 0 && phase > -math.Pi)) 1 else -1)
  //            val IT12 = new Complex(cosP, sinP)
  //            val IT3 = (1 + z / r) / r
  //            val IT = IT12 * IT3 * rw.miLE
  //            IT
  //          })
  //          pixels(captureYI)(captureXI) = its.sum
  //        }(ec)
  //        futures += future
  //      }
  //    }
  //    futures.foreach(future => Await.result(future, 1 hour))
  //    pixels
  //  }

  def captureIntensity(wavefront: Wavefront) = {
    val amplitude = captureAmplitudeWithRoundedWavefront(wavefront)
    Util.tic("done capture")
    val intensities = new Array[Array[Double]](grid)
    Range(0, grid).map(i => intensities(i) = Array.fill(grid)(0.0))
    for (captureXI <- 0 until grid) {
      for (captureYI <- 0 until grid) {
        val amp = amplitude(captureYI)(captureXI)
        intensities(captureYI)(captureXI) = (amp * amp.conjugate).real
      }
    }
    intensities
  }

  def captureImage(wavefront: Wavefront) = {
    val intensities = captureIntensity(wavefront)
    val image = new BufferedImage(grid, grid, BufferedImage.TYPE_INT_ARGB)
    val allI = intensities.flatten
    val min = allI.min
    val max = allI.max * 1.00001
    for (captureXI <- 0 until grid) {
      for (captureYI <- 0 until grid) {
        val intensity = intensities(captureYI)(captureXI)
        val gray = ((intensity - min) / (max - min) * 256).toInt
        image.setRGB(captureXI, captureYI, new Color(gray, gray, gray).getRGB)
      }
    }
    image
  }

  private def roundWavefront(wavefront: Wavefront) = {
    val buffer = new ArrayBuffer[RoundWavefront]()
    val EArray = wavefront.EArray
    val wavefrontGrid = EArray.size
    val wavefrontXs = wavefront.xs
    val wavefrontYs = wavefront.ys
    for (wavefrontXI <- 0 until wavefrontGrid) {
      val wavefrontX = wavefrontXs(wavefrontXI)
      for (wavefrontYI <- 0 until wavefrontGrid) {
        val wavefrontY = wavefrontYs(wavefrontYI)
        if (math.sqrt(wavefrontX * wavefrontX + wavefrontY * wavefrontY) <= wavefront.size / 2)
          buffer += new RoundWavefront(wavefrontX, wavefrontY, EArray(wavefrontYI)(wavefrontXI),
            (n - 1) * (math.sqrt(R * R - wavefrontX * wavefrontX - wavefrontY * wavefrontY) - R + d) + d)
      }
    }
    buffer.toList
  }

  class RoundWavefront(val x: Double, val y: Double, val E: Complex, val phaseLens: Double) {
    val miLE = (-1) * Complex.i / 2 / lambda * E
  }

}