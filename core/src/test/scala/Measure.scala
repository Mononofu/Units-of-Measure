package macros

import macroimpl._
import MeasureImpl.u
import units._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.math.BigDecimal

import Numeric.Implicits._

class MeasureSpec extends FlatSpec with ShouldMatchers {
  val n = new BigDecimal(new java.math.BigDecimal(10))
  "A generic Measure" should "work for any instance of Numeric" in {
    u(n, "m*s") should equal (new Measure[ CUnit[SUnit[Meter, Pos1], SUnit[Second, Pos1]], BigDecimal](n))
  }

}
