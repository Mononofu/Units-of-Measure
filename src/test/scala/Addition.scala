package macros

import macroimpl._
import MeasuredNumberImpl.u
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class AdditionSpec extends FlatSpec with ShouldMatchers {
  "Adding two numbers" should "only work if they have the same units" in {
    (u(10, "m*s") + u(15, "m*s")) should equal (u(25, "m s"))
  }

}
