package com.hwaipy.optics

import java.awt.Color
import java.awt.image.BufferedImage

import scala.collection.mutable.ListBuffer

object Util {
  def toImage(data: Array[Array[Double]]) = {
    val image = new BufferedImage(data(0).size, data.size, BufferedImage.TYPE_INT_ARGB)
    val allI = data.flatten
    val min = allI.min
    val max = allI.max * 1.00001
    for (captureXI <- 0 until data(0).size) {
      for (captureYI <- 0 until data.size) {
        val intensity = data(captureYI)(captureXI)
        val gray = ((intensity - min) / (max - min) * 256).toInt
        image.setRGB(captureXI, captureYI, new Color(gray, gray, gray).getRGB)
      }
    }
    image
  }

  val ticPositions = new ListBuffer[Tuple2[String, Long]]()

  def tic(position: String) = ticPositions += ((position, System.nanoTime))

  def toc = {
    val startPoint = ticPositions.head._2
    ticPositions.foreach(ticPosition => {
      val time = (ticPosition._2 - startPoint) / 1e6
      println(s"${ticPosition._1}\t\t\t$time ms")
    })
  }
}
