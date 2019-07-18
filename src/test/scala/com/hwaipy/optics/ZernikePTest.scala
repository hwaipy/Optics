package com.hydra.optics

import com.hwaipy.sim.ZernikeP
import org.scalatest._

class MessageTest extends FunSuite with BeforeAndAfter {
  before {
  }

  test("ZernikeP Calculate Test") {
    val checkingPointsInMatlab = List(
      (1, 1), (1, 51), (51, 1), (51, 51), (101, 181), (181, 101), (99, 199), (199, 99)
    )
    val checkingPoints = checkingPointsInMatlab.map(z => (z._2 - 1, z._1 - 1))
    val expecteds = Map(
      1 -> List(-1.4142, -1.4142, -0.7036, -0.7036, 0.0071, 1.1442, -0.0213, 1.4000),
      2 -> List(-1.4142, -0.7036, -1.4142, -0.7036, 1.1442, 0.0071, 1.4000, -0.0213),
      3 -> List(2.4495, 1.2186, 1.2186, 0.6062, 0.0100, 0.0100, -0.0366, -0.0366),
      4 -> List(1.7321, 0.4287, 0.4287, -0.8747, -0.5983, -0.5983, -0.0342, -0.0342),
      5 -> List(-0.0000, -0.9216, 0.9216, -0.0000, 0.8016, -0.8016, 1.2000, -1.2000),
      6 -> List(-2.0000, 0.2575, -1.3693, -0.2463, 0.0099, -0.5295, -0.0443, -0.9695),
      7 -> List(-2.0000, 0.2575, 0.1281, 1.2512, -0.0102, -1.6474, 0.0160, -1.0487),
      8 -> List(-2.0000, 0.1281, 0.2575, 1.2512, -1.6474, -0.0102, -1.0487, 0.0160),
      10 -> List(-0.0000, -1.1838, 1.1838, -0.0000, 0.0084, -0.0084, -0.0462, 0.0462),
      15 -> List(2.4495, 0.7157, -0.7879, 0.0746, 0.0066, 0.2122, -0.0443, 0.5809),
      20 -> List(2.4495, -0.7879, 0.7157, 0.0746, 0.2122, 0.0066, 0.5809, -0.0443),
      33 -> List(2.8284, -1.0499, 0.1974, 1.3448, 1.8203, -0.0339, 0.4676, 0.0214),
      48 -> List(-3.1623, 0.2101, -1.1172, 1.1799, -0.0107, 0.5740, -0.0698, -1.5265),
      63 -> List(0.0000, 0.6341, -0.6341, 0.0000, 1.5209, -1.5209, 1.9364, -1.9364),
    )
    expecteds.foreach(expected => {
      val zernikeP = ZernikeP(expected._1, 200).array
      val actual = checkingPoints.map(cp => zernikeP(cp._1)(cp._2))
      actual.zip(expected._2).foreach(z => assert(near(z._1, z._2)))
    })

    //assert(mi.messageID == id0 + 1 + i)
  }

  private def near(d1: Double, d2: Double, threashold: Double = 1e-4) = math.abs(d1 - d2) < threashold

  //test("Test get information") {
  //  val m = Message.wrap(map)
  //  assert(m.get[String]("keyString").get == "value1")
  //  intercept[ClassCastException] {
  //    m.get[Int]("keyString").get + 1
  //  }
  //  intercept[IllegalArgumentException] {
  //    m.get[String]("keyNull", false)
  //  }
  //  assert(m.get[String]("keyNull", true) == None)
  //}
  //
  //test("Test basic information") {
  //  val b = Message.newBuilder
  //  assert(b.create.to == None)
  //  b.to(null)
  //  assert(b.create.to == None)
  //  b.to("the target")
  //  assert(b.create.to.get == "the target")
  //}
  //
  //test("Test type and content") {
  //  val builder = Message.newBuilder
  //  assert(builder.create.messageType == Unknown)
  //  val m1 = builder.asRequest("TestRequest1").create
  //  assert(m1.messageType == Request)
  //  assert(m1.requestContent == ("TestRequest1", Nil, Map()))
  //  intercept[IllegalArgumentException] {
  //    builder.asRequest("TestRequest2", List(100, "arg"), Map("a" -> 1, Message.KeyTo -> "11")).create
  //  }
  //  val m2 = builder.asRequest("TestRequest2", List(100, "arg"), Map("a" -> 1, "b" -> "bb")).create
  //  assert(m2.messageType == Request)
  //  assert(m2.requestContent == ("TestRequest2", 100 :: "arg" :: Nil, Map("b" -> "bb", "a" -> 1)))
  //  val m3 = builder.asResponse("ContentOfResponse", 100).create
  //  assert(m3.messageType == Response)
  //  intercept[IllegalStateException] {
  //    m3.requestContent
  //  }
  //  assert(m3.responseContent == ("ContentOfResponse", 100))
  //  val m4 = builder.asError("ContentOfError", 1001).create
  //  assert(m4.messageType == Error)
  //  intercept[IllegalStateException] {
  //    m4.requestContent
  //  }
  //  intercept[IllegalStateException] {
  //    m4.responseContent
  //  }
  //  assert(m4.errorContent == ("ContentOfError", 1001))
  //}
  //
  //test("Test update message.") {
  //  val m1 = Message.newBuilder.asRequest("TestRequest1").create
  //  assert(m1.get[Int]("testitem") == None)
  //  val m2 = m1 + ("testitem" -> 100)
  //  assert(m2.get[Int]("testitem").get == 100)
  //  val m3 = m2.builder.create
  //  assert(m3.get[Int]("testitem").get == 100)
  //  val m4 = (m3.builder += ("t2" -> "11")).create
  //  assert(m4.get[Int]("testitem").get == 100)
  //  assert(m4.get[String]("t2").get == "11")
  //  val r1 = m4.response(99)
  //  assert(r1.messageType == Response)
  //  assert(r1.responseContent == (99, m4.messageID))
  //  val e1 = m4.error(999)
  //  assert(e1.messageType == Error)
  //  assert(e1.errorContent == (999, m4.messageID))
  //}
}

