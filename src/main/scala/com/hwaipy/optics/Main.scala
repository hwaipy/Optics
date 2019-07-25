package com.hwaipy.optics

import java.io.File
import java.util.concurrent.Executors

import javax.imageio.ImageIO

import scala.concurrent.ExecutionContext

object SIM extends App {

  //  val n = 1.5
  //  val d = 3
  //  val RL = 25
  //  val f = RL / (n - 1)
  //  val R0 = 1
  //  val lambda = 1.550e-3
  //  val k = 2 * math.Pi / lambda
  //  val phy = lambda / 2 / R0
  //  val fried_parameter = 100
  //  val telescope_diameter = 1000
  //  val mr0 = 400
  //  val ne0 = mr0
  //  val pixelSize = 11
  //  val R = 3 * R0
  //  val zernike_patten_number = 1
  //  val zernike_level = 6

  private val e = Executors.newFixedThreadPool(1)
  implicit private val ec = ExecutionContext.fromExecutor(e)

  Util.tic("start")
  val grid = 70
  val wavefront = Wavefront.plane(grid = grid, size = 2)
  val capture = new Capture(50, 50, grid = grid, size = 0.5f)
  val atmosphese = new TurbulencedAtomosphese(64, 2, grid = grid, telescopeDiameter = 1000, friedParameter = 100)
  val turbulencedWavefront = atmosphese.turbulence(wavefront, Array[Double](0, 0, 0, 0, 0, 0, 3))
  Util.tic("done init")
  //  val image = capture.captureImage(turbulencedWavefront)
  Util.tic("finish")
  capture.testTensorFlow()
  Util.toc
  //  ImageIO.write(image, "png", new File("test.png"))
  e.shutdown()


  //  OpenCLTest.test()
}