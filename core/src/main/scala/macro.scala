package macros

import macroimpl._
import MeasureImpl.u
import units._
import Units._

import scala.reflect.runtime.universe.{ WeakTypeTag, Expr }

object Main extends App {

  /*def add[T: WeakTypeTag](n: Measure[T], that: Measure[T]): Measure[T] =
    macro MeasureImpl.addition_impl[T]*/

  /* Measure als Value class implementieren ! */
  val a = u(2, "m")     // new Measure[Meter](2)
  val b = u(3, "m")     // new Measure[Meter](3)
  val c = u(4, "s")     // new Measure[Second](4)
  val d = u(5, "m*s")   // new Measure[Times[Meter, Second]](5)
  val e = u(10, "m*s/s") // new Measure[Divide[Times[Second, Meter], Second]](10)
  val f = u(32, "m*s*s") // new Measure[Times[Second, Times[Second, Meter]]](32)
  val g = new Measure[CUnit[SUnit[Meter, Pos1], SUnit[Second, Pos2]]](10)
  //val f = u(10, "m/s")

  val test: u("m*s") = u(10, "m*s")


  println("\n==== raw numbers")
  println("a: " + a)
  println("automatic simplification:")
  println("u(10, 'm*s/s'): " + u(10, "m*s/s"))

  println("\n==== addition")
  val tmp: u("m") = a + b
  println(s"$a + $b: " + (a + b))
  println("ugly manually defined unit is reduced too:")
  println(s"$g + $f: " + (g + f) )


  // type checker catches unit mismatches
  //println(d + e)
  // u(5, "m") + u(4, "s")

  (new Measure[CUnit[SUnit[Meter, Pos1], SUnit[Second, Pos1]]](1) +
   new Measure[CUnit[SUnit[Meter, Pos1], SUnit[Second, Pos1]]](2))

  println("\n==== multiplication")
  println(s"$b * $c: ${b * c}")
  println(s"$e * $c: ${e * c}")

  println("10 m/s * 32 s: " + u(10, "m/s") * u(32, "s"))

  println("\n==== combined")
  println(s"$d + ($b * $c): ${d + (b * c)}")
  println(s"$d + ($b * $c): ${d + (b * c)}")

  val result: u("m*s") = b * c

  println("\n==== access units with macro trickery")
  println(s"${b.unit}")
  println(s"${c.unit}")
  println(s"${d.unit}")
  println(s"${e.unit}")
  println(s"${f.unit}")

  // type error
  // println("e + (b * c): " + (e + (b * c)))
  //
  val bench3 = new BenchMeasure()
  bench3.bench(40, 1000000)
  val bench2 = new BenchIntFlat()
  bench2.bench(40, 1000000)
}
