package macroimpl

import language.experimental.macros
import scala.reflect.macros.Context
import collection.mutable.ListBuffer
import scala.reflect.runtime.universe.{WeakTypeTag, TypeRef, TypeTag}

class MeasureDouble[T](val n: Double) extends AnyVal {
  override def toString = n.toString

  def +[U](that: MeasureDouble[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureDoubleImpl.addition_impl[T, U]
  def -[U](that: MeasureDouble[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureDoubleImpl.subtraction_impl[T, U]
  def *[U](that: MeasureDouble[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureDoubleImpl.multiplication_impl[T, U]
  def /[U](that: MeasureDouble[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureDoubleImpl.division_impl[T, U]
  def **[U](that: Int)(implicit tag: WeakTypeTag[T]) =
    macro MeasureDoubleImpl.exponentiation_impl[T]


  def toInt = n.toInt
  def toLong = n.toLong
  def toFloat = n.toFloat
  def toDouble = n

  def asInt = new MeasureInt[T](n.toInt)
  def asLong = new MeasureLong[T](n.toLong)
  def asFloat = new MeasureFloat[T](n.toFloat)
  def asDouble = this

  def as(unitEx: String)(implicit tag: WeakTypeTag[T]) = macro MeasureImpl.as_impl[T]
  def unit(implicit tag: WeakTypeTag[T]) = macro MeasureImpl.get_unit_impl[T]
}

import Helpers._

object MeasureDoubleImpl {
    def addition_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasureDouble[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val resultType = enforceUnitEquality(c)(tag.actualType, that.actualType)
    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new MeasureDouble[$resultType]($aID.n + $bID.n)"))
  }

  def subtraction_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasureDouble[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val resultType = enforceUnitEquality(c)(tag.actualType, that.actualType)
    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new MeasureDouble[$resultType]($aID.n - $bID.n)"))
  }

  def multiplication_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasureDouble[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val typeA = parseType(c)(tag.actualType)
    val typeB = parseType(c)(that.actualType)

    val resultType = combine(typeA ++ typeB).toTree(c)

    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new MeasureDouble[$resultType]($aID.n * $bID.n)"))
  }

  def division_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasureDouble[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {
    import c.universe._

    val typeA = parseType(c)(tag.actualType)
    val typeB = parseType(c)(that.actualType)

    val resultType = combine(typeA ++ typeB.map(u => SUnit(u.name, -u.power))).toTree(c)

    val comp = new Precomputer[c.type](c)
    val (aID, bID) = (comp.compute(c.prefix.tree), comp.compute(that.tree))

    c.Expr(Block(comp.evals.toList, q"new MeasureDouble[$resultType]($aID.n / $bID.n)"))
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

    c.Expr(Block(comp.evals.toList, q"new MeasureDouble[$resultType]($exp)"))
  }
}
