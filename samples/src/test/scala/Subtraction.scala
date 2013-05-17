package macros

import macroimpl._
import MeasureImpl.u
import units._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class SubtractionSpec extends FlatSpec with ShouldMatchers {
  "Subtracting two numbers" should "only work if they have the same units" in {
    (u(10, "m*s") - u(15, "m*s")) should equal (u(-5, "m s"))
  }

}
