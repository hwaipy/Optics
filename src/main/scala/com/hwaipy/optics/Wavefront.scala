package com.hwaipy.optics

import breeze.linalg._
import breeze.math.Complex

object Wavefront {
  def plane(size: Double = 1, grid: Int = 200) =
    new Wavefront(
      Range(0, grid).toArray.map(sub => Range(0, grid).toArray.map(_ => Complex.one)),
      size)

  def gaussian(sigma: Double = 0.3, size: Double = 1, grid: Int = 200) =
    new Wavefront(
      Range(0, grid).toArray.map(yi => Range(0, grid).toArray.map(xi => {
        val x = (xi / grid.toDouble - 1.0 / 2) * size
        val y = (yi / grid.toDouble - 1.0 / 2) * size
        Complex(math.exp(-(x * x + y * y) / (sigma * sigma)), 0)
      })),
      size)
}

class Wavefront(private val E: Array[Array[Complex]], val size: Double) {
  def EArray = E.map(sub => sub.map(d => d))

  def xs = linspace(-size / 2, size / 2, E(0).size).data

  def ys = linspace(-size / 2, size / 2, E.size).data
}