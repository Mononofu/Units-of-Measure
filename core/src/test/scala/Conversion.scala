package macros

import macroimpl._
import MeasureImpl.u
import units._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ConversionSpec extends FlatSpec with ShouldMatchers {
  "Converting a Measure" should "adjust it's value" in {
    u(10, "m*s").as("ft*s") should equal (u(32, "ft*s"))
    u(10, "km").as("m") should equal (u(10000, "m"))
    u(10, "ft*m").as("m*m") should equal (u(3, "m^2"))
    u(2, "min").as("s") should equal (u(120, "s"))
  }

  it should "resolve units recursively" in {
    u(1, "h").as("s") should equal (u(3600, "s"))
  }

  it should "work with complex base units" in {
    u(1, "N").as("dyn") should equal (u(100000, "dyn"))
  }

}
