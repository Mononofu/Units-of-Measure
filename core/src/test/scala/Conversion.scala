package macros

import macroimpl._
import MeasureImpl.u
import units._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ConversionSpec extends FlatSpec with ShouldMatchers {
  "Converting a Measure" should "adjust it's value" in {
    u(10, "m*s").as("ft*s") should equal (u(32, "ft*s"))
  }

}
