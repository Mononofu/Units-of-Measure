package macros

import macroimpl._
import MeasureImpl.{u, u_i}
import units._

object Main extends App {
  val a = u(2, "m")
  val b = u(3, "m")
  val h = u(4, "s")
  val d = u(5, "m*s")
  val e = u(10, "m*s/s")
  val f = u(32, "m*s*s")
  val g = new Measure[CUnit[SUnit[Meter, Pos1], SUnit[Second, Pos2]], Int](10)

  val test: u_i("m*s") = u(10, "m*s")

  println("\n==== raw numbers")
  println("a: " + a)
  println("automatic simplification:")
  println("u(10, 'm*s/s'): " + u(10, "m*s/s"))

  println("\n==== addition")
  val tmp: u_i("m") = a + b
  println(s"$a + $b: " + (a + b))
  println("ugly manually defined unit is reduced too:")
  println(s"$g + $f: " + (g.asInt + f) )


  // type checker catches unit mismatches
  //println(d + e)
  // u(5, "m") + u(4, "s")


  println("\n==== multiplication")
  println(s"$b * $h: ${b * h}")
  println(s"$e * $h: ${e * h}")

  println("10 m/s * 32 s: " + u(10, "m/s") * u(32, "s"))

  println("\n==== combined")
  println(s"$d + ($b * $h): ${d + (b * h)}")
  println(s"$d + ($b * $h): ${d + (b * h)}")

  val result: u_i("m*s") = b * h

  println("\n==== access units with macro trickery")
  println(s"${b.unit}")
  println(s"${h.unit}")
  println(s"${d.unit}")
  println(s"${e.unit}")
  println(s"${f.unit}")

  // type error
  // println("e + (b * h): " + (e + (b * h)))

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


  val c = u(2.998e8, "m/s")
  println( (u(1.0, "g") * c * c).as("J") )
}
