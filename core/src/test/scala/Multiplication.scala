package macros

import macroimpl._
import MeasureImpl.u
import units._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class MultiplicationSpec extends FlatSpec with ShouldMatchers {
  "Multiplying two numbers" should "combine their units" in {
    (u(10, "m") * u(15, "s")) should equal (u(150, "m s"))
    (u(10, "m") * u(15, "s")).unit should equal ("m*s")
  }

  it should "cancel out units occurring with opposite signs" in {
    (u(10, "m*s") * u(15, "m/s")) should equal (u(150, "m^2"))
    (u(10, "m*s") * u(15, "m/s")).unit should equal ("m^2")
  }

}
