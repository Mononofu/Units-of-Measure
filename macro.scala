import scala.reflect.runtime.universe.{ WeakTypeTag, Expr }

class MyMeasuredNumber[T: WeakTypeTag](val num: Int) extends MeasuredNumber[T](num) {
  //def my_add(n: MeasuredNumber[T], that: MeasuredNumber[T]): MeasuredNumber[T] =
  //  macro MeasuredNumberImpl.addition_impl[T]

  def +[U](that: MeasuredNumber[U])(implicit tag: WeakTypeTag[U]): MeasuredNumber[T] = 
    macro MeasuredNumberImpl.addition_impl[U]

/*
  def my_multiply[U: WeakTypeTag](n: MeasuredNumber[T], that: MeasuredNumber[U]) =
    macro MeasuredNumberImpl.multiplication_impl[T, U] */

  def *[U](that: MeasuredNumber[U])(implicit tag: WeakTypeTag[U], tag2: WeakTypeTag[T]): MeasuredNumber[Times[T, U]] =
    macro MeasuredNumberImpl.multiplication_impl[T, U]
}

object Main extends App {
  implicit def num2mynum[T: WeakTypeTag](n: MeasuredNumber[T]) = new MyMeasuredNumber[T](n.n)

  /*def add[T: WeakTypeTag](n: MeasuredNumber[T], that: MeasuredNumber[T]): MeasuredNumber[T] =
    macro MeasuredNumberImpl.addition_impl[T]*/

  val a = new MeasuredNumber[Meter](2)
  val b = new MeasuredNumber[Meter](3)
  val c = new MeasuredNumber[Second](4)
  val d = new MeasuredNumber[Times[Meter, Second]](5)
  val e = new MeasuredNumber[Divide[Times[Second, Meter], Second]](10)

  println("\n==== raw numbers")
  println("a: " + a)
  println("e: " + e)

  println("\n==== addition")
  println("a + b: " + (a + b))

  // type error
  //println(d + e)

  println("in place: " + (new MeasuredNumber[Second](1) + new MeasuredNumber[Second](2)))
  

  println("\n==== multiplication")
  println("b * c: " + (b * c))

  println("\n==== combined")
  println("d + (b * c): " + (d + (b * c)))

  // type error
  // println("e + (b * c): " + (e + (b * c)))
}