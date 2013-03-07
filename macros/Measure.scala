package macroimpl

import language.experimental.macros
import scala.reflect.macros.Context
import collection.mutable.ListBuffer
import scala.reflect.runtime.universe.{WeakTypeTag, TypeRef, TypeTag}

// to make it independent of number type, use Numeric typeclass from
// https://github.com/non/spire
// the one from scala is too slow

class Measure[T, N: Numeric](val n: N)(implicit num: Numeric[N]) {
  override def toString = n.toString

  def +[U](that: Measure[U, N])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureGenericImpl.addition_impl[T, U]
  def -[U](that: Measure[U, N])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureGenericImpl.subtraction_impl[T, U]
  def *[U](that: Measure[U, N])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureGenericImpl.multiplication_impl[T, U]
  def /[U](that: Measure[U, N])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureGenericImpl.division_impl[T, U]

  import num.mkNumericOps
  def toInt = n
  def toLong = n.toLong
  def toDouble = n.toDouble
  def toFloat = n.toFloat

  def asInt = new MeasureInt[T](n.toInt)
  def asLong = new MeasureLong[T](n.toInt)
  def asFloat =  new MeasureFloat[T](n.toFloat)
  def asDouble = new MeasureDouble[T](n.toDouble)
  def asType[U: Numeric](conv: N => U) = new Measure[T, U](conv(n))

  def as(unitEx: String)(implicit tag: WeakTypeTag[T]) = macro MeasureImpl.as_impl[T]
  def unit(implicit tag: WeakTypeTag[T]) = macro MeasureImpl.get_unit_impl[T]

  override def equals(other: Any) = other match {
    case that: Measure[T, N] => this.n == that.n
    case _ => false
  }
}

import Helpers._

object MeasureGenericImpl {
def addition_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[Measure[U, _]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val resultType = enforceUnitEquality(c)(tag.actualType, that.actualType)
    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new Measure[$resultType]($aID.n + $bID.n)"))
  }

  def subtraction_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[Measure[U, _]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val resultType = enforceUnitEquality(c)(tag.actualType, that.actualType)
    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new Measure[$resultType]($aID.n - $bID.n)"))
  }

  def multiplication_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[Measure[U, _]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val typeA = parseType(c)(tag.actualType)
    val typeB = parseType(c)(that.actualType)

    val resultType = combine(typeA ++ typeB).toTree(c)

    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new Measure[$resultType]($aID.n * $bID.n)"))
  }

  def division_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[Measure[U, _]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val typeA = parseType(c)(tag.actualType)
    val typeB = parseType(c)(that.actualType)

    val resultType = combine(typeA ++ typeB.map(u => SUnit(u.name, -u.power))).toTree(c)

    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new Measure[$resultType]($aID.n / $bID.n)"))
  }

}
