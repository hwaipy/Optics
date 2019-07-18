package com.hwaipy.optics

import breeze.math.Complex

class Capture(val f: Double, val distance: Double, halfSize: Double = 0.5, pixel: Int = 200) {
  val z = f - distance
  val z2 = z * z
  val xs = linspace(-halfSize, halfSize, pixel)
  val ys = linspace(-halfSize, halfSize, pixel)
  val lambda = 1.550e-3
  val k = 2 * math.Pi / lambda
  val n = 1.5
  val RL = 25
  val d = 3
  val deta = 3 * R0 / (mr0 - 1) * 2 * math.Pi / (ne0 - 1) //积分面积微元

  def capture(wavefront: Wavefront) = {
    val wavefrontArray = wavefront.phaseArray
    val E3 = wavefrontArray.map(row => row.map(cell => (Complex(math.cos(cell), math.sin(cell)))))
    for (wfxi <- 0 until E3.size) {
      val wfx = wavefront.xs(wfxi)
      for (wfyi <- 0 until E3(0).size) {
        val wfy = wavefront.ys(wfyi)
        val rho0 = math.sqrt(wfx * wfx + wfy * wfy)
        for (capxi <- 0 until pixel) {
          val capx = xs(capxi)
          for (capyi <- 0 until pixel) {
            val capy = ys(capyi)
            val Rrho = math.sqrt(math.pow(wfx - capx, 2) + math.pow(wfy - capy, 2) + z2)
            val Rtheta = z / Rrho
            val phi = k * ((n - 1) * (math.sqrt(RL * RL - rho0 * rho0) - (RL - d)) + d)
            val opd = Complex(math.cos(phi), math.sin(phi))
            val Ep = Complex(0, -1) / lambda / 2 * Complex(math.cos(Rrho * k), math.sin(Rrho * k)) * (1 + Rtheta) / Rrho //*deta.*rho0.*opd.*E1.*E3;
            //%             Ep=-1j/lambda/2*exp(Rrho*1j*k).*(1+Rtheta)./Rrho*deta.*rho0.*opd.*E1.*E3;
          }
        }
        //%             E2(gk,df)=sum(Ep(:));
      }
    }
  }

  private def linspace(start: Double, stop: Double, count: Int) = Range(0, count).map(_ / (count - 1.0) * (stop - start) + start).toList
}