import scala.reflect.runtime.universe.{ WeakTypeTag, Expr }

abstract class Meter
abstract class Second


object Main extends App {

  /*def add[T: WeakTypeTag](n: MeasuredNumber[T], that: MeasuredNumber[T]): MeasuredNumber[T] =
    macro MeasuredNumberImpl.addition_impl[T]*/

  /* MeasuredNumber als Value class implementieren ! */
  val a = new MeasuredNumber[Meter](2)
  val b = new MeasuredNumber[Meter](3)
  val c = new MeasuredNumber[Second](4)
  val d = new MeasuredNumber[Times[Meter, Second]](5)
  val e = new MeasuredNumber[Divide[Times[Second, Meter], Second]](10)
  val f = new MeasuredNumber[Times[Second, Times[Second, Meter]]](32)
  val g = new MeasuredNumber[Divide[Times[Second, Meter],
                             Divide[Meter, Times[Second, Meter]]]](10)
  //val f = u(10, "m/s")

  import MeasuredNumberImpl.u

  println("\n==== raw numbers")
  println("a: " + a)
  println("e: " + e)

  println("tt: " + (g + f) )

  println("\n==== addition")
  val tmp: MeasuredNumber[Meter] = a + b
  println("a + b: " + (a + b))

  // type error
  //println(d + e)

  (new MeasuredNumber[Times[Second, Meter]](1) +
   new MeasuredNumber[Times[Second, Meter]](2))


  println("\n==== multiplication")
  println("b * c: " + (b * c))
  println("b * c * c: " + (e * c))

  println("\n==== combined")
  println("d + (b * c): " + (d + (b * c)))

  val test = u(10, "m/s")
  println(test)

  val result: MeasuredNumber[Times[Meter, Second]] = b * c

  // type error
  // println("e + (b * c): " + (e + (b * c)))
}
