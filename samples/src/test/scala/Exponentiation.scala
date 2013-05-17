package macros

import macroimpl._
import MeasureImpl.u
import units._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ExponentiationSpec extends FlatSpec with ShouldMatchers {
  "Raising a Measure to a power" should "work if it's an integer power" in {
    (u(10, "m*s") ** 2) should equal (u(100, "m^2*s^2"))
    (u(10, "m*s") ** 2).unit should equal ("m^2*s^2")

    (u(15l, "m*s") ** 2) should equal (u(225l, "m^2*s^2"))
    (u(15l, "m*s") ** 2).unit should equal ("m^2*s^2")

    (u(15.0f, "m*s") ** 2) should equal (u(225.0f, "m^2*s^2"))
    (u(15.0f, "m*s") ** 2).unit should equal ("m^2*s^2")

    (u(15.0, "m*s") ** 2) should equal (u(225.0, "m^2*s^2"))
    (u(15.0, "m*s") ** 2).unit should equal ("m^2*s^2")
  }


}
