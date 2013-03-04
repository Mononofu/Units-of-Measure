package macros

import macroimpl._
import MeasureImpl.u
import units._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class MeasureFormatSpec extends FlatSpec with ShouldMatchers {
  "Multiple measures" should "be seperable with *" in {
    u(10, "m*s").unit should equal ((new MeasureInt[ CUnit[SUnit[Meter, Pos1], SUnit[Second, Pos1]] ](10)).unit)
  }

  it should "be seperable with spaces" in {
    u(10, "m s").unit should equal ((new MeasureInt[ CUnit[SUnit[Meter, Pos1], SUnit[Second, Pos1]] ](10)).unit)
  }

  "Division" should "be possible explicitly" in {
    u(10, "m/s").unit should equal ((new MeasureInt[ CUnit[SUnit[Meter, Pos1], SUnit[Second, Neg1]] ](10)).unit)
  }

  it should "be possible implicitly" in {
    u(10, "m s^-1").unit should equal ((new MeasureInt[ CUnit[SUnit[Meter, Pos1], SUnit[Second, Neg1]] ](10)).unit)
  }

  it should "be chainable" in {
    u(10, "m/s/s").unit should equal ((new MeasureInt[ CUnit[SUnit[Meter, Pos1], SUnit[Second, Neg2]] ](10)).unit)
  }

  it should "be its own inverse" in {
    u(10, "1/(1/m)").unit should equal ((new MeasureInt[ SUnit[Meter, Pos1] ](10)).unit)
    u(10, "1/(1/(1/m))").unit should equal ((new MeasureInt[ SUnit[Meter, Neg1] ](10)).unit)
    u(10, "1/(1/(1/(1/m)))").unit should equal ((new MeasureInt[ SUnit[Meter, Pos1] ](10)).unit)
  }

  "Exponentiation" should "ignore spaces" in {
    u(10, "m^2/s^2").unit should equal ((u(10, "m ^ 2 / s ^ 2")).unit)
    u(10, "s^-1").unit should equal ((new MeasureInt[ SUnit[Second, Neg1] ](10)).unit)
    u(10, "m^2/s^2").unit should equal ((new MeasureInt[ CUnit[SUnit[Meter, Pos2], SUnit[Second, Neg2]] ](10)).unit)
  }

}
