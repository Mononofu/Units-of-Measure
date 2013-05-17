package macros

import macroimpl._
import MeasureImpl.u
import units._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class DivisonSpec extends FlatSpec with ShouldMatchers {
  "Dividing two numbers" should "combine their units" in {
    (u(10, "m") / u(2, "s")) should equal (u(5, "m/s"))
    (u(10, "m") / u(2, "s")).unit should equal ("m / s")
  }

  it should "cancel out units occurring with same signs" in {
    (u(10, "m*s") / u(2, "m/s")) should equal (u(5, "s^2"))
    (u(10, "m*s") / u(2, "m/s")).unit should equal ("s^2")
  }

}
