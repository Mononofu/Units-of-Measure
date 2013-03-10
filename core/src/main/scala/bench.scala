package macros

import macroimpl._
import MeasureImpl.{u, u_i, u_d}
import units._

object Utility {
  def gc() = {
    var obj = new Object();
    val ref = new java.lang.ref.WeakReference[Object](obj);
    obj = null;
    while(ref.get() != null) {
      System.gc();
    }
  }
}

abstract class Bench {
  def init(length: Int)
  def destroy()
  def runAdd(length: Int)
  def runMul(length: Int)
  def bench(runs: Int, length: Int, silent: Boolean = false) = {
    if(!silent) println(s"\tBenchmarking ${this.getClass}")
    init(length)
    (1 to 5).foreach(_ => runAdd(length))
    // throw away fastest and slowest 5%
    val timingsAddRaw = (1 to runs).map(_ => time { runAdd(length) } )
    // filter out runs slowed down by GC
    val timingsAdd = timingsAddRaw.filter(_ < 2 * timingsAddRaw.sum / runs)
    val perOpAdd = timingsAdd.sum * 1e6 / (timingsAdd.length * length)
    if(!silent) println(f"$perOpAdd%5.2f ns per addition,       fastest run ${timingsAdd.min}, slowest ${timingsAdd.max}")

    (1 to 5).foreach(_ => runMul(length))
    val timingsMulRaw = (1 to runs).map(_ => time { runMul(length) } )
    val timingsMul = timingsMulRaw.filter(_ < 2 * timingsMulRaw.sum / runs)
    val perOpMul = timingsMul.sum * 1e6 / (timingsMul.length * length)
    if(!silent) println(f"$perOpMul%5.2f ns per multiplication, fastest run ${timingsMul.min}, slowest ${timingsMul.max}")
    if(!silent) println()
    destroy()
    (timingsAdd, timingsMul)
  }

  def time(f: => Unit): Long = {
    val start = compat.Platform.currentTime
    f
    compat.Platform.currentTime - start
  }
}


// annoying: can't put Measure[T] or the typetag breaks ...
case class Vector4M[T](val x: MeasureInt[SUnit[T, Pos1]], val y: MeasureInt[SUnit[T, Pos1]],
  val z: MeasureInt[SUnit[T, Pos1]], val w: MeasureInt[SUnit[T, Pos1]]) {
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

  def destroy() {
    a = null
    b = null
    c = null
    d = null
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


case class Vector4D(val x: Double, val y: Double, val z: Double, val w: Double) {
  def +(that: Vector4D) = Vector4D(this.x + that.x, this.y + that.y,
    this.z + that.z, this.w + that.w)

  def *(that: Vector4D) =
    this.x * that.x + this.y * that.y + this.z * that.z + this.w * that.w
}


class BenchDoubleFlat extends Bench {
  var a: Array[ Vector4D ] = _
  var b: Array[ Vector4D ] = _
  var c: Array[ Vector4D ] = _
  var d: Array[Double] = _

  def init(length: Int) {
    a = (1 to length).map(i => Vector4D(i, i, i, i)).toArray
    b = (1 to length).map(i => Vector4D(i, i, i, i)).toArray
    c = new Array[ Vector4D ](length)
    d = new Array[Double](length)
  }

  def destroy() {
    a = null
    b = null
    c = null
    d = null
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
  var d: Array[ MeasureInt[ SUnit[Meter, Pos2] ] ] = _

  def init(length: Int) {
    a = new Array[ Vector4M[ Meter ] ](length)
    b = new Array[ Vector4M[ Meter ] ](length)
    c = new Array[ Vector4M[ Meter ] ](length)
    d = new Array[ MeasureInt[ SUnit[Meter, Pos2] ] ](length)

    var i = 0
    while(i < length) {
      a(i) = Vector4M(u(i, "m"), u(i, "m"), u(i, "m"), u(i, "m"))
      b(i) = Vector4M(u(i, "m"), u(i, "m"), u(i, "m"), u(i, "m"))
      i+=1
    }
  }

  def destroy() {
    a = null
    b = null
    c = null
    d = null
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



class BenchMeasureIntMacro extends Bench {
  var a: Array[ Vector4G[u_i("m")] ] = _
  var b: Array[ Vector4G[u_i("m")]  ] = _
  var c: Array[ Vector4G[u_i("m")]  ] = _
  var d: Array[ u_i("m^2") ] = _

  def init(length: Int) {
    a = new Array[ Vector4G[u_i("m")]  ](length)
    b = new Array[ Vector4G[u_i("m")]  ](length)
    c = new Array[ Vector4G[u_i("m")]  ](length)
    d = new Array[ u_i("m^2") ](length)

    var i = 0
    while(i < length) {
      a(i) = Vector4G(u(i, "m"), u(i, "m"), u(i, "m"), u(i, "m"))
      b(i) = Vector4G(u(i, "m"), u(i, "m"), u(i, "m"), u(i, "m"))
      i+=1
    }
  }

  def destroy() {
    a = null
    b = null
    c = null
    d = null
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

class BenchMeasureDoubleMacro extends Bench {
  var a: Array[ Vector4GD[u_d("m")] ] = _
  var b: Array[ Vector4GD[u_d("m")]  ] = _
  var c: Array[ Vector4GD[u_d("m")]  ] = _
  var d: Array[ u_d("m^2") ] = _

  def init(length: Int) {
    a = new Array[ Vector4GD[u_d("m")]  ](length)
    b = new Array[ Vector4GD[u_d("m")]  ](length)
    c = new Array[ Vector4GD[u_d("m")]  ](length)
    d = new Array[ u_d("m^2") ](length)

    var i = 0.0
    while(i < length) {
      a(i.toInt) = Vector4GD(u(i, "m"), u(i, "m"), u(i, "m"), u(i, "m"))
      b(i.toInt) = Vector4GD(u(i, "m"), u(i, "m"), u(i, "m"), u(i, "m"))
      i+=1
    }
  }

  def destroy() {
    a = null
    b = null
    c = null
    d = null
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
