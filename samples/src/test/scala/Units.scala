package macros

import macroimpl._
import MeasureImpl.u
import units._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class UnitsSpec extends FlatSpec with ShouldMatchers {
  "Angle measurements" should "not have units" in {
    u(3.1416, "rad").as("deg").toDouble should be (180.0 plusOrMinus 0.001)
    u(3.1416, "rad").as("1").toDouble should equal (3.1416)
  }
}
