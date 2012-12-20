package macros

import macroimpl._
import MeasuredNumberImpl.u
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class MeasureFormatSpec extends FlatSpec with ShouldMatchers {
  "Multiple measures" should "be seperable with *" in {
    u(10, "m*s") should equal (new MeasuredNumber[Times[Meter, Second]](10))
  }

  it should "be seperable with spaces" in {
    u(10, "m s") should equal (new MeasuredNumber[Times[Meter, Second]](10))
  }

  "Division" should "be possible explicitly" in {
    u(10, "m/s") should equal (new MeasuredNumber[Divide[Meter, Second]](10))
  }

  it should "be possible implicitly" in {
    u(10, "m s^-1") should equal (new MeasuredNumber[Divide[Meter, Second]](10))
  }

  it should "be chainable" in {
    u(10, "m/s/s") should equal (new MeasuredNumber[Divide[Meter, Times[Second, Second]]](10))
  }

  "Exponentiation" should "ignore spaces" in {
    u(10, "m^2/s^2") should equal (u(10, "m ^ 2 / s ^ 2"))
    u(10, "s^-1") should equal (new MeasuredNumber[Divide[Unit, Second]](10))
    u(10, "m^2/s^2") should equal (new MeasuredNumber[Divide[Times[Meter, Meter], Times[Second, Second]]](10))
  }

}
