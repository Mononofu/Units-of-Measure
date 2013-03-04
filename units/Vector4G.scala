package units

import macroimpl._
import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.runtime.universe.{WeakTypeTag, TypeRef, TypeTag}
import collection.mutable.ListBuffer

// the type bound here is very important, otherwise the value class will be boxed
// (ie T is assumed to be an object), and performance is abysmal.
// check it with 'javap Vector4G' to see the byte code
case class Vector4G[T <: MeasureInt[_]](val x: T, val y: T, val z: T, val w: T) {
  def +(that: Vector4G[T]) = macro VectorImpl.plus_impl[T]
  def *(that: Vector4G[T]) = macro VectorImpl.times_impl[T]
}

object VectorImpl {
  def plus_impl[T <: MeasureInt[_]](c: Context)(that: c.Expr[Vector4G[T]]): c.Expr[Any]  = {
    import c.universe._
    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))
    val stats = q"Vector4G($aID.x + $bID.x, $aID.y + $bID.y, $aID.z + $bID.z, $aID.w + $bID.w)"
    c.Expr(Block(comp.evals.toList, stats))
  }

  def times_impl[T <: MeasureInt[_]](c: Context)(that: c.Expr[Vector4G[T]]): c.Expr[Any]  = {
    import c.universe._
    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))
    val stats = q"$aID.x * $bID.x + $aID.y * $bID.y + $aID.z * $bID.z + $aID.w * $bID.w"
    c.Expr(Block(comp.evals.toList, stats))
  }
}
