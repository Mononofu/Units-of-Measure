package macros

import macroimpl._
import MeasureImpl.u
import units._

object Main extends App {
  val a = u(2, "m")
  val b = u(3, "m")
  val c = u(4, "s")
  val d = u(5, "m*s")
  val e = u(10, "m*s/s")
  val f = u(32, "m*s*s")
  val g = new Measure[CUnit[SUnit[Meter, Pos1], SUnit[Second, Pos2]]](10)

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

  val numRuns = 20
  val runSize = 1000000

  def gc() = {
    var obj = new Object();
    val ref = new java.lang.ref.WeakReference[Object](obj);
    obj = null;
    while(ref.get() != null) {
      System.gc();
    }
  }

  new BenchMeasure().bench(numRuns, runSize)
  gc()
  new BenchIntFlat().bench(numRuns, runSize)
  gc()
  new BenchMeasureMacro().bench(numRuns, runSize)
}
