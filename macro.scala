import scala.reflect.runtime.universe.{ WeakTypeTag, Expr }

abstract class Meter
abstract class Second


object Main extends App {

  import MeasuredNumberImpl.u
  /*def add[T: WeakTypeTag](n: MeasuredNumber[T], that: MeasuredNumber[T]): MeasuredNumber[T] =
    macro MeasuredNumberImpl.addition_impl[T]*/

  /* MeasuredNumber als Value class implementieren ! */
  val a = u(2, "m")     // new MeasuredNumber[Meter](2)
  val b = u(3, "m")     // new MeasuredNumber[Meter](3)
  val c = u(4, "s")     // new MeasuredNumber[Second](4)
  val d = u(5, "m*s")   // new MeasuredNumber[Times[Meter, Second]](5)
  val e = u(10, "m*s/s") // new MeasuredNumber[Divide[Times[Second, Meter], Second]](10)
  val f = u(32, "m*s*s") // new MeasuredNumber[Times[Second, Times[Second, Meter]]](32)
  val g = new MeasuredNumber[Divide[Times[Second, Meter],
                             Divide[Meter, Times[Second, Meter]]]](10)
  //val f = u(10, "m/s")

  val test: MeasuredNumber[Times[Meter, Second]] = u(10, "m*s")


  println("\n==== raw numbers")
  println("a: " + a)
  println("automatic simplification:")
  println("u(10, 'm*s/s'): " + u(10, "m*s/s"))

  println("\n==== addition")
  val tmp: MeasuredNumber[Meter] = a + b
  println(s"$a + $b: " + (a + b))
  println("ugly manually defined unit is reduced too:")
  println(s"$g + $f: " + (g + f) )


  // type checker catches unit mismatches
  //println(d + e)
  // u(5, "m") + u(4, "s")

  (new MeasuredNumber[Times[Second, Meter]](1) +
   new MeasuredNumber[Times[Second, Meter]](2))

  println("\n==== multiplication")
  println(s"$b * $c: ${b * c}")
  println(s"$e * $c: ${e * c}")

  println("10 m/s * 32 s: " + u(10, "m/s") * u(32, "s"))

  println("\n==== combined")
  println(s"$d + ($b * $c): ${d + (b * c)}")
  println(s"$d + ($b * $c): ${d + (b * c)}")

  val result: MeasuredNumber[Times[Meter, Second]] = b * c

  // type error
  // println("e + (b * c): " + (e + (b * c)))
}
