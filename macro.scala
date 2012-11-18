import scala.reflect.runtime.universe.WeakTypeTag

case class MyNumber[T: WeakTypeTag](val num: Number[T]) {
  def my_add(n: Number[T], that: Number[T]): Number[T] =
    macro NumberImpl.addition_impl[T]
  def +(that: Number[T]) = my_add(num, that)


  def my_multiply[U: WeakTypeTag](n: Number[T], that: Number[U]) =
    macro NumberImpl.multiplication_impl[T, U]
  def *[U: WeakTypeTag](that: Number[U]) =
    my_multiply(num, that)
}

object Main extends App {
  implicit def num2mynum[T: WeakTypeTag](n: Number[T]) = MyNumber[T](n)

  val a = Number[Meter](2)
  val b = Number[Meter](3)
  val c = Number[Second](4)
  val d = Number[Times[Meter, Second]](5)
  val e = Number[Divide[Times[Second, Meter], Second]](10)

  println(a)
  println(e)

  println(a + (a + b))
  println(b * c)
  println(d + (b * c))
}