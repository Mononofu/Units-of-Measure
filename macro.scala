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

  println(a)
  println(e)

  println(a + b)

  // type error
  //println(d + e)

  println(new MeasuredNumber[Second](1) + new MeasuredNumber[Second](2))
  
  println(b * c)
  //println(d + (b * c))
}