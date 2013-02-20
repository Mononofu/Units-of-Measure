package macros

import macroimpl._
import MeasureImpl.u
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class MeasureFormatSpec extends FlatSpec with ShouldMatchers {
  "Multiple measures" should "be seperable with *" in {
    u(10, "m*s") should equal (new Measure[ CUnit[SUnit[Meter, Pos1], SUnit[Second, Pos1]] ](10))
  }

  it should "be seperable with spaces" in {
    u(10, "m s") should equal (new Measure[ CUnit[SUnit[Meter, Pos1], SUnit[Second, Pos1]] ](10))
  }

  "Division" should "be possible explicitly" in {
    u(10, "m/s") should equal (new Measure[ CUnit[SUnit[Meter, Pos1], SUnit[Second, Neg1]] ](10))
  }

  it should "be possible implicitly" in {
    u(10, "m s^-1") should equal (new Measure[ CUnit[SUnit[Meter, Pos1], SUnit[Second, Neg1]] ](10))
  }

  it should "be chainable" in {
    u(10, "m/s/s") should equal (new Measure[ CUnit[SUnit[Meter, Pos1], SUnit[Second, Neg2]] ](10))
  }

  it should "be its own inverse" in {
    u(10, "1/(1/m)") should equal (new Measure[ SUnit[Meter, Pos1] ](10))
    u(10, "1/(1/(1/m))") should equal (new Measure[ SUnit[Second, Neg1] ](10))
    u(10, "1/(1/(1/(1/m)))") should equal (new Measure[ SUnit[Meter, Pos1] ](10))
  }

  "Exponentiation" should "ignore spaces" in {
    u(10, "m^2/s^2") should equal (u(10, "m ^ 2 / s ^ 2"))
    u(10, "s^-1") should equal (new Measure[ SUnit[Second, Neg1] ](10))
    u(10, "m^2/s^2") should equal (new Measure[ CUnit[SUnit[Meter, Pos2], SUnit[Second, Neg2]] ](10))
  }

}
