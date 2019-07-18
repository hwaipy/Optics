package com.hwaipy.optics

import scala.collection.mutable

object ZernikeP {
  private val map = new mutable.WeakHashMap[String, ZernikeP]()

  def apply(index: Int, halfSize: Double = 1, grid: Int = 200) = map.getOrElseUpdate(s"$index,$grid", {
    new ZernikeP(index, halfSize, grid)
  })
}

class ZernikeP(val index: Int, halfSize: Double = 1, val grid: Int = 200) {
  val array = Range(0, grid).map(i => Range(0, grid).map(_ => 0.0).toArray).toArray
  val xs = linspace(-halfSize, halfSize, grid)
  val ys = linspace(-halfSize, halfSize, grid)
  private val n = math.ceil((-3 + math.sqrt(9 + 8 * index)) / 2).toInt
  private val m = n + (index - (n + 3) * n / 2) * 2
  private val deltam = if (m == 0) 1 else 0
  private val Norm = math.sqrt(2 * (n + 1) / (1 + deltam))

  Range(0, (n - math.abs(m)) / 2 + 1).foreach(s => {
    val flag = if (s % 2 == 0) 1 else -1
    val prodA = prod(1, (n - s))
    val prodB = prod(1, s)
    val prodC = prod(1, ((n + math.abs(m)) / 2 - s))
    val prodD = prod(1, ((n - math.abs(m)) / 2 - s))
    Range(0, grid).foreach(ix => Range(0, grid).foreach(iy => {
      val x = xs(ix)
      val y = ys(iy)
      val rho = math.sqrt(x * x + y * y)
      array(ix)(iy) += flag * prodA * math.pow(rho, n - 2 * s) / (prodB * prodC * prodD)
    }))
  })

  Range(0, grid).foreach(ix => Range(0, grid).foreach(iy => {
    val x = xs(ix)
    val y = ys(iy)
    val theta =
      if (x == 0) {
        if (y >= 0) math.Pi / 2
        else -math.Pi / 2
      } else if (y == 0) {
        if (x >= 0) 0
        else math.Pi
      } else math.atan(y / x) + (if (x > 0) 0 else math.Pi)
    array(ix)(iy) = (if (m < 0) -Norm * math.sin(m * theta) else Norm * math.cos(m * theta)) * array(ix)(iy)
  }))

  def std(telescopeDiameter: Double, friedParameter: Double) = {
    val variance = 2.246 * (n + 1) * gamma(n - 5 / 6.0) * math.pow((telescopeDiameter / friedParameter), (5 / 3.0)) / (gamma(17 / 6.0) * gamma(17 / 6.0) * gamma(n + 23 / 6.0))
    math.sqrt(variance)
  }

  private def gamma(x: Double): Double = {
    val p = Seq(676.5203681218851, -1259.1392167224028, 771.32342877765313,
      -176.61502916214059, 12.507343278686905, -0.13857109526572012,
      9.9843695780195716e-6, 1.5056327351493116e-7)
    if (x < 0.5) {
      math.Pi / (math.sin(math.Pi * x) * gamma(1 - x))
    } else {
      val x2 = x - 1
      val t = x2 + 7 + 0.5
      val a = p.zipWithIndex.foldLeft(0.99999999999980993)((r, v) => r + v._1 / (x2 + v._2 + 1))
      math.sqrt(2 * math.Pi) * math.pow(t, x2 + 0.5) * math.exp(-t) * a
    }
  }

  private def prod(start: Int, stop: Int) = Range(start, stop + 1).fold(1)((a, b) => a * b)

  private def linspace(start: Double, stop: Double, count: Int) = Range(0, count).map(_ / (count - 1.0) * (stop - start) + start).toList
}