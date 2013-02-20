package macros

import macroimpl._
import MeasureImpl.u

import scala.reflect.runtime.universe.{ WeakTypeTag, Expr }

abstract class Meter
abstract class Second
abstract class Foot {
  val perMeter = u(3, "ft/m")
}

abstract class Bench {
  def init(length: Int)
  def runAdd(length: Int)
  def runMul(length: Int)
  def bench(runs: Int, length: Int) {
    println(s"\tBenchmarking ${this.getClass}")
    init(length)
    (1 to 10).foreach(_ => runAdd(length))
    // throw away fastest and slowest 5%
    val timingsAdd = (1 to runs).map(_ => time { runAdd(length) } ).sorted.take(
      (runs * 0.95).toInt).drop((runs * 0.05).toInt)
    val perOpAdd = timingsAdd.sum * 1e6 / (timingsAdd.length * length)
    println(s"$perOpAdd ns per addition, fastest run ${timingsAdd.min}, slowest ${timingsAdd.max}")

    (1 to 10).foreach(_ => runMul(length))
    val timingsMul = (1 to runs).map(_ => time { runMul(length) } ).sorted.take(
      (runs * 0.95).toInt).drop((runs * 0.05).toInt)
    val perOpMul = timingsMul.sum * 1e6 / (timingsMul.length * length)
    println(s"$perOpMul ns per multiplication, fastest run ${timingsMul.min}, slowest ${timingsMul.max}")
  }

  def time(f: => Unit): Long = {
    val start = compat.Platform.currentTime
    f
    compat.Platform.currentTime - start
  }
}


case class Vector4M[T](val x: Measure[T], val y: Measure[T], val z: Measure[T], val w: Measure[T]) {
  def +(that: Vector4M[T]) = Vector4M(this.x + that.x, this.y + that.y,
    this.z + that.z, this.w + that.w)

  def *(that: Vector4M[T]) =
    this.x * that.x + this.y * that.y + this.z * that.z + this.w * that.w
}

case class Vector4I(val x: Int, val y: Int, val z: Int, val w: Int) {
  def +(that: Vector4I) = Vector4I(this.x + that.x, this.y + that.y,
    this.z + that.z, this.w + that.w)

  def *(that: Vector4I) =
    this.x * that.x + this.y * that.y + this.z * that.z + this.w * that.w
}


class BenchIntFlat extends Bench {
  var a: Array[ Vector4I ] = _
  var b: Array[ Vector4I ] = _
  var c: Array[ Vector4I ] = _
  var d: Array[Int] = _

  def init(length: Int) {
    a = (1 to length).map(i => Vector4I(i, i, i, i)).toArray
    b = (1 to length).map(i => Vector4I(i, i, i, i)).toArray
    c = new Array[ Vector4I ](length)
    d = new Array[Int](length)
  }

  def runAdd(length: Int) {
    var i = 0
    while(i < length) {
      c(i) = a(i) + b(i)
      i+=1
    }
  }

  def runMul(length: Int) {
    var i = 0
    while(i < length) {
      d(i) = a(i) * b(i)
      i+=1
    }
  }
}


class BenchMeasure extends Bench {
  var a: Array[ Vector4M[ Meter ] ] = _
  var b: Array[ Vector4M[ Meter ] ] = _
  var c: Array[ Vector4M[ Meter ] ] = _
  var d: Array[ Measure[ Times[Meter, Meter] ] ] = _

  def init(length: Int) {
    a = new Array[ Vector4M[ Meter ] ](length)
    b = new Array[ Vector4M[ Meter ] ](length)
    c = new Array[ Vector4M[ Meter ] ](length)
    d = new Array[ Measure[ Times[Meter, Meter] ] ](length)

    var i = 0
    while(i < length) {
      a(i) = Vector4M(u(i, "m"), u(i, "m"), u(i, "m"), u(i, "m"))
      b(i) = Vector4M(u(i, "m"), u(i, "m"), u(i, "m"), u(i, "m"))
      i+=1
    }
  }

  def runAdd(length: Int) {
    var i = 0
    while(i < length) {
      c(i) = a(i) + b(i)
      i+=1
    }
  }

  def runMul(length: Int) {
    var i = 0
    while(i < length) {
      d(i) = a(i) * b(i)
      i+=1
    }
  }
}


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
  val g = new Measure[Divide[Times[Second, Meter],
                             Divide[Meter, Times[Second, Meter]]]](10)
  //val f = u(10, "m/s")

  val test: Measure[Times[Meter, Second]] = u(10, "m*s")


  println("\n==== raw numbers")
  println("a: " + a)
  println("automatic simplification:")
  println("u(10, 'm*s/s'): " + u(10, "m*s/s"))

  println("\n==== addition")
  val tmp: Measure[Meter] = a + b
  println(s"$a + $b: " + (a + b))
  println("ugly manually defined unit is reduced too:")
  println(s"$g + $f: " + (g + f) )


  // type checker catches unit mismatches
  //println(d + e)
  // u(5, "m") + u(4, "s")

  (new Measure[Times[Second, Meter]](1) +
   new Measure[Times[Second, Meter]](2))

  println("\n==== multiplication")
  println(s"$b * $c: ${b * c}")
  println(s"$e * $c: ${e * c}")

  println("10 m/s * 32 s: " + u(10, "m/s") * u(32, "s"))

  println("\n==== combined")
  println(s"$d + ($b * $c): ${d + (b * c)}")
  println(s"$d + ($b * $c): ${d + (b * c)}")

  val result: Measure[Times[Meter, Second]] = b * c

  // type error
  // println("e + (b * c): " + (e + (b * c)))
  //
  val bench3 = new BenchMeasure()
  bench3.bench(40, 1000000)
  val bench2 = new BenchIntFlat()
  bench2.bench(40, 1000000)
}
