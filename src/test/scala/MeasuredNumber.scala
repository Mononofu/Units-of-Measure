package macros

import macroimpl._
import MeasuredNumberImpl.u
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class MeasuredNumberSpec extends FlatSpec with ShouldMatchers {
  "Two numbers with the same value but different units" should "not be equal" in {
    u(10, "m*s") should not equal (new MeasuredNumber[Times[Second, Second]](10))
    u(5, "s*s") should not equal (u(10, "m"))
  }

}
