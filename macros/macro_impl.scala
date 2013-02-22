package macroimpl

import language.experimental.macros
import scala.reflect.macros.Context
import collection.mutable.ListBuffer
import scala.reflect.runtime.universe.{WeakTypeTag, TypeRef, TypeTag}

object Helpers {
  def simplify(units: Seq[GeneralUnit]): Seq[GeneralUnit] = {
    def reduceUnits(a: GeneralUnit, b: GeneralUnit) = SUnit(a.name, a.power + b.power)

    val unitGroups = units.groupBy(_.name).toList
    val unitList = for((_, units) <- unitGroups) yield units.reduce(reduceUnits)
    unitList.sortBy(_.name)
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

  /*def paramInfo(implicit tag: WeakTypeTag[Measure[T]]) = {
    val targs = tag.tpe match { case TypeRef(_, _, args) => args }
    val units = TypeParser.parse(targs(0).toString)
    val reducedUnits = reduce(units.simplify)
    //s"$n ${units.toString}, raw: ${targs(0)} - ${units.simplify} - $reducedUnits"
    s"$n $units"
  }*/

  def +[U](that: Measure[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureImpl.addition_impl[T, U]

  def *[U](that: Measure[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasureImpl.multiplication_impl[T, U]

  /*override def equals(other: Any) = other match {
    case that: Measure[T] => this.toString == that.toString
    case _ => false
  }*/
}

abstract class Dimension
abstract class Pos1 extends Dimension
abstract class Pos2 extends Dimension
abstract class Pos3 extends Dimension
abstract class Neg1 extends Dimension
abstract class Neg2 extends Dimension
abstract class Neg3 extends Dimension
abstract class Zero extends Dimension

abstract class GeneralUnit {
  def name: String = throw new Exception(s"invalid state, name called on $this")
  def power: Int = throw new Exception(s"invalid state, power called on $this")
  def invert: GeneralUnit = throw new Exception(s"invalid state, invert called on $this")
  def toTree(c: Context): c.universe.Tree =
    throw new Exception(s"invalid state, toTree called on $this")
}
case class SUnit[U, D <: Dimension](override val name: String, override val power: Int = 1) extends GeneralUnit {
  override def toString = name
  override def equals(that: Any) = that match {
    case SUnit(n, p) => n == name && p == power
    case _ => false
  }
  def dimName = power match {
    case d if d > 0 => "Pos" + d
    case 0 => "Zero"
    case d => "Neg" + (-d)
  }

  override def toTree(c: Context): c.universe.Tree = c.universe.AppliedTypeTree(
          c.universe.Ident(c.universe.newTypeName("SUnit")),
          List(c.universe.Ident(c.universe.newTypeName(name)),
               c.universe.Ident(c.universe.newTypeName(dimName))))

  override def invert = SUnit(name, -power)
}
case class CUnit[U, V](unit: GeneralUnit, next: GeneralUnit) extends GeneralUnit {
  override def toTree(c: Context): c.universe.Tree = c.universe.AppliedTypeTree(
          c.universe.Ident(c.universe.newTypeName("CUnit")),
          List(unit.toTree(c), next.toTree(c)))
}


import scala.util.parsing.combinator._

object UnitParser extends JavaTokenParsers with PackratParsers {
  def parse(in: String, c: Context) = parseAll(term, in) match {
    case Success(r, _) => combine(r).toTree(c)
    case _ => c.abort(c.enclosingPosition, s"unknown units and/or invalid format '$in'")
  }

  def toTypenames(units: List[UnitParser.~[String, List[GeneralUnit]]]): List[GeneralUnit] = units match {
    case Nil => Nil
    case ("*"~x) :: xs => x ++ toTypenames(xs)
    case (""~x) :: xs => x ++ toTypenames(xs)
    case ("/"~x) :: xs => x.map(_.invert) ++ toTypenames(xs)
  }

  def power(t: List[GeneralUnit], n: Int): List[GeneralUnit] = n match {
    case 1 => t
    case 0 => List(SUnit("Unit"))
    case -1 => t.map(_.invert)
    case n if n > 0 => t ++ power(t, n - 1)
    case _ => t.map(_.invert) ++ power(t, n + 1)
  }

  lazy val term: PackratParser[List[GeneralUnit]] =
    longFactor~rep(("*"|"/"|"")~longFactor) ^^ {
      case t~Nil => t
      case t~l => t ++ toTypenames(l)
    }
  lazy val longFactor: PackratParser[List[GeneralUnit]] = (
      shortFactor~"^"~wholeNumber ^^ { case t~"^"~n => power(t, n.toInt) }
    | shortFactor
    )
  lazy val shortFactor: PackratParser[List[GeneralUnit]] = (
      unitname ^^ { case u => List(SUnit(u)) }
    | "("~>term<~")"
    )

  lazy val unitname: PackratParser[String] = (
      "m" ^^ { _ => "Meter" }
    | "s" ^^ { _ => "Second" }
    | "c" ^^ { _ => "Gram" }
    | "w" ^^ { _ => "Watt" }
    | "ft" ^^ { _ => "Foot" }
    | "1" ^^ { _ => "Unit" }
    )
}

object TypeParser extends JavaTokenParsers {
  def parse(in: String) = parseAll(typename, in) match {
    case Success(r, _) => simplify(r)
  }

  def dimValue(dim: String) = dim match {
    case "Zero" => 0
    case d if d.startsWith("Pos") => d.substring(3).toInt
    case d => d.substring(3).toInt * -1
  }

  // grammar
  def typename: Parser[List[GeneralUnit]] = (
      "macroimpl.CUnit["~typename~","~typename~"]" ^^ {
        case "macroimpl.CUnit["~t~","~u~"]" => t ++ u
      }
    | "macroimpl.SUnit["~id~","~id~"]" ^^ {
      case "macroimpl.SUnit["~u~","~d~"]" => List(SUnit(u, dimValue(d)))
    }
    | id~"["~typename~"]" ^^ { case n~"["~t~"]" => t }
    )

  def id: Parser[String] = rep1sep(ident, ".") ^^ ( _.last )
}

object MeasureImpl {

  def u(nEx: Int, unitEx: String) = macro u_impl

  def u_impl(c: Context)
        (nEx: c.Expr[Int], unitEx: c.Expr[String]): c.Expr[Any] = {
    import c.universe._

    //println(showRaw(c))

    val evals = ListBuffer[ValDef]()

    def _precompute(value: Tree, tpe: Type): Ident = {
      val freshName = newTermName(c.fresh("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }

    val nID = _precompute(nEx.tree, typeOf[Int])

    val unit = unitEx match {
      case Expr(Literal(Constant(s))) => s.toString
      case _ => c.abort(c.enclosingPosition, "unit has to be a constant string")
    }

    //println(unit)
    val parsedUnit = UnitParser.parse(unit, c)
    //println(showRaw(parsedUnit))

    println(showRaw(c.topLevelDef(c.universe.newTypeName("units.Meter"))))

    val stats = Apply(Select(New(AppliedTypeTree(
      Ident(newTypeName("Measure")),
      List( parsedUnit )
      )), nme.CONSTRUCTOR),
    List(Ident(newTermName(nID.toString))))

    //println(stats)
    //println(showRaw(stats))

    c.Expr(Block(evals.toList, stats))
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

    println(tag.actualType.toString)
    val typeA = parseType(c)(tag.actualType)
    val typeB = parseType(c)(that.actualType)

    if(typeA != typeB)
      c.abort(c.enclosingPosition, s"type error, $typeA != $typeB")

    val resultType = combine(typeA).toTree(c)

    //println(s"result type is $resultType")

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
