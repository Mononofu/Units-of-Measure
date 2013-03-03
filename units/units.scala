package units

import macroimpl._
import CreateUnitMacros.MyUnit

trait Meter extends MyUnit("Meter", "m")
trait Kilometer extends MyUnit("Kilometer", "km", (1000, "m"))
trait Centimeter extends MyUnit("Centimeter", "cm", (0.01, "m"))

trait Second extends MyUnit("Second", "s")
trait Minute extends MyUnit("Minute", "min", (60, "s"))
trait Hour extends MyUnit("Hour", "h", (60, "min"))

trait Foot extends MyUnit("Foot", "ft", (0.3048, "m"))

trait Gram extends MyUnit("Gram", "g")
trait Kilogram extends MyUnit("Kilogram", "kg", (1000, "g"))

trait Newton extends MyUnit("Newton", "N", (1, "kg*m/s^2"))
trait Dyne extends MyUnit("Dyne", "dyn", (1, "g*cm/s^2"))


import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.runtime.universe.{WeakTypeTag, TypeRef, TypeTag}
import collection.mutable.ListBuffer

case class Vector4G[T <: Measure[_]](val x: T, val y: T, val z: T, val w: T) {
  def +(that: Vector4G[T]) = macro VectorImpl.plus_impl[T]
  def *(that: Vector4G[T]) = macro VectorImpl.times_impl[T]
}

object VectorImpl {
  def precompute(c: Context)(a: c.Tree, b: c.Tree) = {
    import c.universe._
    val evals = ListBuffer[ValDef]()

    def _precompute(value: Tree, tpe: Type): Ident = {
      val freshName = newTermName(c.fresh("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }

    val aID = _precompute(a, a.tpe)
    val bID = _precompute(b, b.tpe)
    (evals, aID, bID)
  }

  def plus_impl[T <: Measure[_]](c: Context)(that: c.Expr[Vector4G[T]]): c.Expr[Any]  = {
    import c.universe._
    val (evals, aID, bID) = precompute(c)(c.prefix.tree, that.tree)
    val stats = q"Vector4G($aID.x + $bID.x, $aID.y + $bID.y, $aID.z + $bID.z, $aID.w + $bID.w)"
    c.Expr(Block(evals.toList, stats))
  }

  def times_impl[T <: Measure[_]](c: Context)(that: c.Expr[Vector4G[T]]): c.Expr[Any]  = {
    import c.universe._
    val (evals, aID, bID) = precompute(c)(c.prefix.tree, that.tree)
    val stats = q"$aID.x * $bID.x + $aID.y * $bID.y + $aID.z * $bID.z + $aID.w * $bID.w"
    c.Expr(Block(evals.toList, stats))
  }
}
