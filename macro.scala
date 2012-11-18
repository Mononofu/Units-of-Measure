
case class MyNumber[T](val num: Number[T]) {
  def my_add(n: Number[T], that: Number[T]): Number[T] =
    macro NumberImpl.addition_impl[T]
  def +(that: Number[T]): Number[T] = my_add(num, that)


  def my_multiply[U](n: Number[T], that: Number[U]) =
    macro NumberImpl.multiplication_impl[T, U]
  def *[U](that: Number[U]) =
    my_multiply(num, that)
}

object Main extends App {
  implicit def num2mynum[T](n: Number[T]) = MyNumber[T](n)

  val a = Number[Meter](2)
  val b = Number[Meter](3)
  val c = Number[Second](4)
  val d = Number[Times[Meter, Second]](5)

  println(a)

  println(a + (a + b))
  println(b * c)
  println(d + (b * c))
}