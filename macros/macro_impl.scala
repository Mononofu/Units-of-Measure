package macroimpl

import language.experimental.macros
import scala.reflect.macros.Context
import collection.mutable.ListBuffer
import scala.reflect.runtime.universe.{WeakTypeTag, TypeRef, TypeTag}

object Helpers {
  val packageName = "$$units$$"

  def simplify(units: Seq[GeneralUnit]): Seq[GeneralUnit] = {
    def reduceUnits(a: GeneralUnit, b: GeneralUnit) = SUnit(a.name, a.power + b.power)

    val unitGroups = units.groupBy(_.name).toList
    val unitList = for((_, units) <- unitGroups) yield units.reduce(reduceUnits)
    unitList.filter(_.power != 0).sortBy(_.name)
  }

  def combine(units: Seq[GeneralUnit]): GeneralUnit = {
    def makeTypes(next: GeneralUnit, sum: GeneralUnit) = CUnit(next, sum)
    val unitList = simplify(units)
    unitList match {
      case unit :: Nil => unit
      case xs => xs.reduceRight(makeTypes)
    }
  }
}

import Helpers._


// to make it independent of number type, use Numeric typeclass from
// https://github.com/non/spire
// the one from scala is too slow
class Measure[T](val n: Int) extends AnyVal {
  override def toString = n.toString

  def +[U](that: Measure[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureImpl.addition_impl[T, U]

  def *[U](that: Measure[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureImpl.multiplication_impl[T, U]

  def toInt = n.toInt
  def toDouble = n.toDouble
  def toFloat = n.toFloat

  def as(unitEx: String)(implicit tag: WeakTypeTag[T]) = macro MeasureImpl.as_impl[T]

  def unit(implicit tag: WeakTypeTag[T]) = macro MeasureImpl.get_unit_impl[T]
}




object MeasureImpl {
  def u(nEx: Int, unitEx: String) = macro u_impl

  type u(unitEx: String) = macro u_unit_impl

  def compute_unit(c: Context)(unitEx: c.Expr[String]) = {
    import c.universe._

    val unit = unitEx match {
      case Expr(Literal(Constant(s))) => s.toString
      case _ => c.abort(c.enclosingPosition, "unit has to be a constant string")
    }

    UnitParser[c.type](c).parse(unit)
  }

  def u_impl(c: Context)
        (nEx: c.Expr[Int], unitEx: c.Expr[String]): c.Expr[Any] = {
    import c.universe._
    val evals = ListBuffer[ValDef]()

    def _precompute(value: Tree, tpe: Type): Ident = {
      val freshName = newTermName(c.fresh("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }

    val nID = _precompute(nEx.tree, typeOf[Int])
    val parsedUnit = compute_unit(c)(unitEx)

    val stats = Apply(Select(New(AppliedTypeTree(
      Ident(newTypeName("Measure")),
      List( parsedUnit )
      )), nme.CONSTRUCTOR),
    List(Ident(newTermName(nID.toString))))

    c.Expr(Block(evals.toList, stats))
  }

  def u_unit_impl(c: Context)(unitEx: c.Expr[String]): c.Tree = {
    val parsedUnit = compute_unit(c)(unitEx)
    import c.universe._
    AppliedTypeTree( Ident(newTypeName("Measure")), List( parsedUnit ) )
  }

  def as_impl[T: c.WeakTypeTag]
    (c: Context)(unitEx: c.Expr[String])(tag: c.Expr[WeakTypeTag[T]]): c.Expr[Any] = {
    /* TODO: add conversion */
    val parsedUnit = compute_unit(c)(unitEx)
    import c.universe._

    val evals = ListBuffer[ValDef]()

    def _precompute(value: Tree, tpe: Type): Ident = {
      val freshName = newTermName(c.fresh("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }

    val nID = _precompute(c.prefix.tree, typeOf[Measure[_]])

    val stats = Apply(Select(New(AppliedTypeTree(
      Ident(newTypeName("Measure")),
      List( parsedUnit )
      )), nme.CONSTRUCTOR),
    List(Select(
      Ident(newTermName(nID.toString)),
      newTermName("n"))))

    c.Expr(Block(evals.toList, stats))
  }

  def get_unit_impl[T: c.WeakTypeTag](c: Context)
    (tag: c.Expr[WeakTypeTag[T]]): c.Expr[String] = {

    def prettyUnit(u: GeneralUnit) = {
      val unitSymbol = c.mirror.staticClass(packageName + ".Translate$" + u.name)
      val dummy = unitSymbol.typeSignature
      val annotations = unitSymbol.asClass.annotations
      val short = annotations.find(a => a.tpe == c.universe.typeOf[ShortName]) match {
        case None => c.abort(c.enclosingPosition, s"unknown unit '${u.name}'")
        case Some(a) => a.scalaArgs.head match {
          case c.universe.Literal(c.universe.Constant(shortName)) => shortName match {
            case s: String => s
          }
        }
      }
      if(u.power == 1) short
      else s"$short^${Math.abs(u.power)}"
    }

    val typeA = parseType(c)(tag.actualType)
    val (posUnits, negUnits) = simplify(typeA).partition(u => u.power > 0)
    val posUnitsString = posUnits match {
      case Nil => "1"
      case l => l.map(prettyUnit).mkString("*")
    }
    val negUnitsString = negUnits match {
      case Nil => ""
      case l => " / " + negUnits.map(prettyUnit).mkString
    }
    val unit = posUnitsString + negUnitsString

    import c.universe._
    c.Expr[String](Literal(Constant(unit)))
  }

  def precompute(c: Context)(a: c.Tree, b: c.Tree) = {
    import c.universe._
    val evals = ListBuffer[ValDef]()

    def _precompute(value: Tree, tpe: Type): Ident = {
      val freshName = newTermName(c.fresh("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }

    val aID = _precompute(a, typeOf[Measure[_]])
    val bID = _precompute(b, typeOf[Measure[_]])
    (evals, aID, bID)
  }

  def parseType(c: Context)(tpe: c.Type) = TypeParser.parse(tpe.toString.replace("$", ""))

  def addition_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[Measure[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {

    import c.universe._

    val typeA = parseType(c)(tag.actualType)
    val typeB = parseType(c)(that.actualType)

    if(typeA != typeB)
      c.abort(c.enclosingPosition, s"type error, $typeA != $typeB")

    val resultType = combine(typeA).toTree(c)

    val (evals, aID, bID) = precompute(c)(that.tree, c.prefix.tree)

    val stats = Apply(Select(New(AppliedTypeTree(
      Ident(newTypeName("Measure")),
      List( resultType )
      )), nme.CONSTRUCTOR),
      List(Apply(Select(
        Select(
          Ident(newTermName(aID.toString)),
          newTermName("n")),
        newTermName("$plus")),
          List(
            Select(
              Ident(newTermName(bID.toString)),
              newTermName("n"))))))


    c.Expr(Block(evals.toList, stats))
  }

  def multiplication_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[Measure[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {

    import c.universe._

    val typeA = parseType(c)(tag.actualType)
    val typeB = parseType(c)(that.actualType)

    val resultType = combine(typeA ++ typeB).toTree(c)


    val (evals, aID, bID) = precompute(c)(that.tree, c.prefix.tree)

    val stats = Apply(Select(New(AppliedTypeTree(
      Ident(newTypeName("Measure")),
      List( resultType )
      )), nme.CONSTRUCTOR),
      List(Apply(Select(
        Select(
          Ident(newTermName(aID.toString)),
          newTermName("n")),
        newTermName("$times")),
          List(
            Select(
              Ident(newTermName(bID.toString)),
              newTermName("n"))))))

    c.Expr(Block(evals.toList, stats))
  }
}
