package macroimpl

import language.experimental.macros
import scala.reflect.macros.Context
import collection.mutable.ListBuffer
import scala.reflect.runtime.universe.{WeakTypeTag, TypeRef, TypeTag}

class MeasureFloat[T](val n: Float) extends AnyVal {
  override def toString = n.toString

  def +[U](that: MeasureFloat[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureFloatImpl.addition_impl[T, U]
  def -[U](that: MeasureFloat[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureFloatImpl.subtraction_impl[T, U]
  def *[U](that: MeasureFloat[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureFloatImpl.multiplication_impl[T, U]
  def /[U](that: MeasureFloat[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureFloatImpl.division_impl[T, U]

  def toInt = n.toInt
  def toLong = n.toLong
  def toFloat = n
  def toDouble = n.toDouble

  def asInt = new MeasureInt[T](n.toInt)
  def asLong = new MeasureLong[T](n.toLong)
  def asFloat = this
  def asDouble = new MeasureDouble[T](n.toDouble)

  def as(unitEx: String)(implicit tag: WeakTypeTag[T]) = macro MeasureImpl.as_impl[T]
  def unit(implicit tag: WeakTypeTag[T]) = macro MeasureImpl.get_unit_impl[T]
}

import Helpers._

object MeasureFloatImpl {
    def addition_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasureFloat[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val resultType = enforceUnitEquality(c)(tag.actualType, that.actualType)
    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new MeasureFloat[$resultType]($aID.n + $bID.n)"))
  }

  def subtraction_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasureFloat[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val resultType = enforceUnitEquality(c)(tag.actualType, that.actualType)
    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new MeasureFloat[$resultType]($aID.n - $bID.n)"))
  }

  def multiplication_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasureFloat[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val typeA = parseType(c)(tag.actualType)
    val typeB = parseType(c)(that.actualType)

    val resultType = combine(typeA ++ typeB).toTree(c)

    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new MeasureFloat[$resultType]($aID.n * $bID.n)"))
  }

  def division_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasureFloat[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val typeA = parseType(c)(tag.actualType)
    val typeB = parseType(c)(that.actualType)

    val resultType = combine(typeA ++ typeB.map(u => SUnit(u.name, -u.power))).toTree(c)

    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new MeasureFloat[$resultType]($aID.n / $bID.n)"))
  }
}
