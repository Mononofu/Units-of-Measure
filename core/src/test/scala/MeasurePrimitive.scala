package macros

import macroimpl._
import MeasureImpl.u
import units._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.math.BigDecimal

import Numeric.Implicits._

class MeasurePrimitiveSpec extends FlatSpec with ShouldMatchers {
  "A Primitive measure" should "only be equal to its own type" in {
    u(2, "m*s") should equal (u(2, "m*s"))
    u(2, "m*s") should not equal (u(2l, "m*s"))
    u(2, "m*s") should not equal (u(2.0f, "m*s"))
    u(2, "m*s") should not equal (u(2.0d, "m*s"))

    u(2l, "m*s") should not equal (u(2, "m*s"))
    u(2l, "m*s") should equal (u(2l, "m*s"))
    u(2l, "m*s") should not equal (u(2.0f, "m*s"))
    u(2l, "m*s") should not equal (u(2.0d, "m*s"))

    u(2.0f, "m*s") should not equal (u(2, "m*s"))
    u(2.0f, "m*s") should not equal (u(2l, "m*s"))
    u(2.0f, "m*s") should equal (u(2.0f, "m*s"))
    u(2.0f, "m*s") should not equal (u(2.0d, "m*s"))

    u(2.0d, "m*s") should not equal (u(2, "m*s"))
    u(2.0d, "m*s") should not equal (u(2l, "m*s"))
    u(2.0d, "m*s") should not equal (u(2.0f, "m*s"))
    u(2.0d, "m*s") should equal (u(2.0d, "m*s"))

    val n = new BigDecimal(new java.math.BigDecimal(2))
    u(2, "m*s") should not equal (u(n, "m*s"))
    u(2l, "m*s") should not equal (u(n, "m*s"))
    u(2.0f, "m*s") should not equal (u(n, "m*s"))
    u(2.0d, "m*s") should not equal (u(n, "m*s"))
  }

  it should "be equal when converted explicitly" in {
    u(2, "m*s").asInt should equal (u(2, "m*s"))
    u(2, "m*s").asLong should equal (u(2l, "m*s"))
    u(2, "m*s").asFloat should equal (u(2.0f, "m*s"))
    u(2, "m*s").asDouble should equal (u(2.0d, "m*s"))

    u(2l, "m*s").asInt should equal (u(2, "m*s"))
    u(2l, "m*s").asLong should equal (u(2l, "m*s"))
    u(2l, "m*s").asFloat should equal (u(2.0f, "m*s"))
    u(2l, "m*s").asDouble should equal (u(2.0d, "m*s"))

    u(2.0f, "m*s").asInt should equal (u(2, "m*s"))
    u(2.0f, "m*s").asLong should equal (u(2l, "m*s"))
    u(2.0f, "m*s").asFloat should equal (u(2.0f, "m*s"))
    u(2.0f, "m*s").asDouble should equal (u(2.0d, "m*s"))

    u(2.0d, "m*s").asInt should equal (u(2, "m*s"))
    u(2.0d, "m*s").asLong should equal (u(2l, "m*s"))
    u(2.0d, "m*s").asFloat should equal (u(2.0f, "m*s"))
    u(2.0d, "m*s").asDouble should equal (u(2.0d, "m*s"))

    val n = new BigDecimal(new java.math.BigDecimal(2))
    u(n, "m*s").asInt should equal (u(2, "m*s"))
    u(n, "m*s").asLong should equal (u(2l, "m*s"))
    u(n, "m*s").asFloat should equal (u(2.0f, "m*s"))
    u(n, "m*s").asDouble should equal (u(2.0d, "m*s"))
  }

}
