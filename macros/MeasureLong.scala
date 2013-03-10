package macroimpl

import language.experimental.macros
import scala.reflect.macros.Context
import collection.mutable.ListBuffer
import scala.reflect.runtime.universe.{WeakTypeTag, TypeRef, TypeTag}

class MeasureLong[T](val n: Long) extends AnyVal {
  override def toString = n.toString

  def +[U](that: MeasureLong[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureLongImpl.addition_impl[T, U]
  def -[U](that: MeasureLong[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureLongImpl.subtraction_impl[T, U]
  def *[U](that: MeasureLong[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureLongImpl.multiplication_impl[T, U]
  def /[U](that: MeasureLong[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureLongImpl.division_impl[T, U]
  def **[U](that: Int)(implicit tag: WeakTypeTag[T]) =
    macro MeasureLongImpl.exponentiation_impl[T]

  def toInt = n.toInt
  def toLong = n
  def toFloat = n.toFloat
  def toDouble = n.toDouble

  def asInt = new MeasureInt[T](n.toInt)
  def asLong = this
  def asFloat =  new MeasureFloat[T](n.toFloat)
  def asDouble = new MeasureDouble[T](n.toDouble)

  def as(unitEx: String)(implicit tag: WeakTypeTag[T]) = macro MeasureImpl.as_impl[T]
  def unit(implicit tag: WeakTypeTag[T]) = macro MeasureImpl.get_unit_impl[T]
}

import Helpers._

object MeasureLongImpl {
    def addition_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasureLong[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val resultType = enforceUnitEquality(c)(tag.actualType, that.actualType)
    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new MeasureLong[$resultType]($aID.n + $bID.n)"))
  }

  def subtraction_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasureLong[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val resultType = enforceUnitEquality(c)(tag.actualType, that.actualType)
    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new MeasureLong[$resultType]($aID.n - $bID.n)"))
  }

  def multiplication_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasureLong[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val typeA = parseType(c)(tag.actualType)
    val typeB = parseType(c)(that.actualType)

    val resultType = combine(typeA ++ typeB).toTree(c)

    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new MeasureLong[$resultType]($aID.n * $bID.n)"))
  }

  def division_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasureLong[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val typeA = parseType(c)(tag.actualType)
    val typeB = parseType(c)(that.actualType)

    val resultType = combine(typeA ++ typeB.map(u => SUnit(u.name, -u.power))).toTree(c)

    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new MeasureLong[$resultType]($aID.n / $bID.n)"))
  }

  def exponentiation_impl[T: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[Int])
    (tag: c.Expr[WeakTypeTag[T]]): c.Expr[Any] = {
    import c.universe._

    val typeA = parseType(c)(tag.actualType)
    val power = that match {
      case Expr(Literal(Constant(n: Int))) => n
      case _ => c.abort(c.enclosingPosition, "can only raise Measure to integral powers")
    }

    val resultType = combine((1 to power).flatMap(_ => typeA)).toTree(c)

    val comp = new Precomputer[c.type](c)
    val aID = comp.compute(c.prefix.tree)
    val exp = if(power == 1) {
      q"$aID.n"
    } else {
      (2 to power).map(_ => q"$aID.n").foldLeft[c.Tree](q"$aID.n")( (a, b) => q"$a * $b")
    }

    c.Expr(Block(comp.evals.toList, q"new MeasureLong[$resultType]($exp)"))
  }
}
