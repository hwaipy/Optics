package com.hwaipy.optics

import scala.util.Random

class WavefrontFactory(telescopeDiameter: Double, friedParameter: Double, halfSize: Double = 1, maxIndex: Int = 64, val grid: Int = 200) {
  val zernikePs = Range(1, maxIndex + 1).map(index => ZernikeP(index, 1, grid))
  val stds = zernikePs.map(zernike => zernike.std(telescopeDiameter, friedParameter))
  val random = new Random(0)

  def generate = {
    val wavefront = new Array[Array[Double]](grid)
    Range(0, grid).map(i => wavefront(i) = new Array[Double](grid))
    val randomWeights = stds.map(std => random.nextGaussian() * std)
    randomWeights.zip(zernikePs).foreach(z => {
      for (ix <- 0 until grid) {
        for (iy <- 0 until grid) {
          val d = z._1 * z._2.array(ix)(iy)
          wavefront(ix)(iy) += d
        }
      }
    })

    def linspace(start: Double, stop: Double, count: Int) = Range(0, count).map(_ / (count - 1.0) * (stop - start) + start).toList

    val xs = linspace(-halfSize, halfSize, grid).toArray
    val ys = linspace(-halfSize, halfSize, grid).toArray
    new Wavefront(wavefront, xs, ys, grid)
  }
}

class Wavefront(val phaseArray: Array[Array[Double]], val xs: Array[Double], val ys: Array[Double], val grid: Int = 200)