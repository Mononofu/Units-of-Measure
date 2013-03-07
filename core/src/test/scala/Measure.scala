package macros

import macroimpl._
import MeasureImpl.u
import units._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.math.BigDecimal

import Numeric.Implicits._

case class Complex(r: Double, i: Double)

class ComplexNumeric extends Numeric[Complex] {
   def plus(x: Complex, y: Complex) = Complex(x.r + y.r, x.i + y.i)
   def fromInt(x: Int): macros.Complex = Complex(x, 0)
   def minus(x: macros.Complex,y: macros.Complex): macros.Complex = ???
   def negate(x: macros.Complex): macros.Complex = ???
   def times(x: macros.Complex,y: macros.Complex): macros.Complex = ???
   def toDouble(x: macros.Complex): Double = x.r.toDouble
   def toFloat(x: macros.Complex): Float = x.r.toFloat
   def toInt(x: macros.Complex): Int = x.r.toInt
   def toLong(x: macros.Complex): Long = x.r.toLong

   // Members declared in scala.math.Ordering
   def compare(x: macros.Complex,y: macros.Complex): Int = ???
}


class MeasureSpec extends FlatSpec with ShouldMatchers {
  implicit def complexNumeric = new ComplexNumeric

  val n = new BigDecimal(new java.math.BigDecimal(10))
  "A generic Measure" should "work for any instance of Numeric" in {
    u(n, "m*s") should equal (new Measure[ CUnit[SUnit[Meter, Pos1], SUnit[Second, Pos1]], BigDecimal](n))
  }

  it should "be convertable to different underlying types" in {
    u(n, "m*s").asType(n => Complex(n.doubleValue(), 0)) should be (u(Complex(10.0, 0), "m*s"))
  }

}
