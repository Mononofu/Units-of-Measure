package macros

import macroimpl._
import MeasureImpl.u
import units._

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
      (runs * 0.90).toInt).drop((runs * 0.1).toInt)
    val perOpAdd = timingsAdd.sum * 1e6 / (timingsAdd.length * length)
    println(f"$perOpAdd%5.2f ns per addition,       fastest run ${timingsAdd.min}, slowest ${timingsAdd.max}")

    (1 to 10).foreach(_ => runMul(length))
    val timingsMul = (1 to runs).map(_ => time { runMul(length) } ).sorted.take(
      (runs * 0.90).toInt).drop((runs * 0.1).toInt)
    val perOpMul = timingsMul.sum * 1e6 / (timingsMul.length * length)
    println(f"$perOpMul%5.2f ns per multiplication, fastest run ${timingsMul.min}, slowest ${timingsMul.max}")
  }

  def time(f: => Unit): Long = {
    val start = compat.Platform.currentTime
    f
    compat.Platform.currentTime - start
  }
}


// annoying: can't put Measure[T] or the typetag breaks ...
case class Vector4M[T](val x: Measure[SUnit[T, Pos1]], val y: Measure[SUnit[T, Pos1]],
  val z: Measure[SUnit[T, Pos1]], val w: Measure[SUnit[T, Pos1]]) {
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
  var d: Array[ Measure[ SUnit[Meter, Pos2] ] ] = _

  def init(length: Int) {
    a = new Array[ Vector4M[ Meter ] ](length)
    b = new Array[ Vector4M[ Meter ] ](length)
    c = new Array[ Vector4M[ Meter ] ](length)
    d = new Array[ Measure[ SUnit[Meter, Pos2] ] ](length)

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



class BenchMeasureMacro extends Bench {
  var a: Array[ Vector4G[u("m")] ] = _
  var b: Array[ Vector4G[u("m")]  ] = _
  var c: Array[ Vector4G[u("m")]  ] = _
  var d: Array[ u("m^2") ] = _

  def init(length: Int) {
    a = new Array[ Vector4G[u("m")]  ](length)
    b = new Array[ Vector4G[u("m")]  ](length)
    c = new Array[ Vector4G[u("m")]  ](length)
    d = new Array[ u("m^2") ](length)

    var i = 0
    while(i < length) {
      a(i) = Vector4G(u(i, "m"), u(i, "m"), u(i, "m"), u(i, "m"))
      b(i) = Vector4G(u(i, "m"), u(i, "m"), u(i, "m"), u(i, "m"))
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
