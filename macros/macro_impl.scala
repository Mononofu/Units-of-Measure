package macroimpl

import language.experimental.macros
import scala.reflect.macros.Context
import collection.mutable.ListBuffer
import scala.reflect.runtime.universe.{WeakTypeTag, TypeRef, TypeTag}


object Helpers {
  def reduce(l: List[Typename]): Typename = {

    def eliminateInverted(l: List[Typename]): List[Typename] = l match {
      case x :: xs => xs.indexOf(Inverted(x).reduce) match {
        case -1 => x :: eliminateInverted(xs)
        case n  => eliminateInverted(xs.slice(0, n) ++ xs.slice(n + 1, xs.length))
      }
      case Nil => Nil
    }

    val units = eliminateInverted(l)

    val (mul, div) = units.partition(x => x match {
      case Inverted(_) => false
      case _ => true
      })

    def toMul(xs: List[Typename]): Typename = xs match {
      case Nil => SimpleType("Unit")
      case x :: Nil => x
      case x :: y :: Nil => Times(x, y)
      case x :: xs => Times(x, toMul(xs))
    }

    div match {
      case Nil => toMul(mul)
      case xs => Divide(toMul(mul), toMul(div.map { case Inverted(u) =>  u }))
    }
  }
}


import Helpers._

class MeasuredNumber[T: WeakTypeTag](val n: Int) {
  override def toString = paramInfo

  def paramInfo(implicit tag: WeakTypeTag[MeasuredNumber[T]]) = {
    val targs = tag.tpe match { case TypeRef(_, _, args) => args }
    val units = TypeParser.parse(targs(0).toString)
    val reducedUnits = reduce(units.simplify)
    //s"$n ${units.toString}, raw: ${targs(0)} - ${units.simplify} - $reducedUnits"
    s"$n $units"
  }

  def +[U](that: MeasuredNumber[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasuredNumberImpl.addition_impl[T, U]

  def *[U](that: MeasuredNumber[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasuredNumberImpl.multiplication_impl[T, U]

  override def equals(other: Any) = other match {
    case that: MeasuredNumber[T] => this.toString == that.toString
    case _ => false
  }
}



abstract class Typename {
  def simplify: List[Typename] = List()
  def toTree(c: Context): c.universe.Tree =
    throw new Exception(s"invalid state, toTree called on $this")
}

case class Inverted(tpe: Typename) extends Typename {
  def reduce = tpe match {
    case Inverted(t) => t
    case _ => this
  }
  override def equals(that: Any) = that match {
    case Inverted(t) => t == tpe
    case _ => false
  }
  override def simplify = tpe match {
    case Times(a, b) => Inverted(a).simplify ++ Inverted(b).simplify
    case Divide(a, b) => a.simplify ++ b.simplify
    case Inverted(t) => t.simplify
    case t => List(this)
  }
}

case class SimpleType(name: String) extends Typename {
  override def toString = name
  override def simplify = List(this)
  override def equals(that: Any) = that match {
    case SimpleType(n) => n == name
    case _ => false
  }
  override def toTree(c: Context): c.universe.Tree = c.universe.Ident(c.universe.newTypeName(name))
}
case class GenericType(name: String, param: Typename) extends Typename


case class Times[T, U](param1: Typename, param2: Typename) extends Typename {
  override def toString = s"$param1 * $param2"
  override def simplify = param1.simplify ++ param2.simplify
  override def toTree(c: Context): c.universe.Tree = c.universe.AppliedTypeTree(
          c.universe.Ident(c.universe.newTypeName("Times")),
          List(param1.toTree(c), param2.toTree(c)))
}

case class Divide[T, U](param1: Typename, param2: Typename) extends Typename {
  override def toString = s"($param1) / ($param2)"
  override def simplify = {
    param1.simplify ++ param2.simplify.map(t => t match {
      case Inverted(t) => t
      case t => Inverted(t)
      })
  }
  override def toTree(c: Context): c.universe.Tree = c.universe.AppliedTypeTree(
          c.universe.Ident(c.universe.newTypeName("Divide")),
          List(param1.toTree(c), param2.toTree(c)))
}



import scala.util.parsing.combinator._

object UnitParser extends JavaTokenParsers with PackratParsers {
  def parse(in: String, c: Context) = parseAll(term, in) match {
    case Success(r, _) => reduce(r.simplify).toTree(c)
    //case _ => c.abort(c.enclosingPosition, s"unknown units and/or invalid format '$in'")
  }

  def toTimes(units: List[Typename]): Typename = units match {
    case x :: Nil => x
    case x :: y :: Nil => Times(x, y)
    case x :: xs => Times(x, toTimes(xs))
  }

  def toTypenames(units: List[UnitParser.~[String, Typename]]): List[Typename] = units match {
    case Nil => Nil
    case ("*"~x) :: xs => x :: toTypenames(xs)
    case (""~x) :: xs => x :: toTypenames(xs)
    case ("/"~x) :: xs => Inverted(x) :: toTypenames(xs)
  }

  def power(t: Typename, n: Int): Typename = n match {
    case 1 => t
    case 0 => SimpleType("1")
    case -1 => Inverted(t)
    case n if n > 0 => Times(t, power(t, n - 1))
    case _ => Times(Inverted(t), power(t, n + 1))
  }

  lazy val term: PackratParser[Typename] =
    longFactor~rep(("*"|"/"|"")~longFactor) ^^ {
      case t~Nil => t
      case t~l => Times(t, toTimes(toTypenames(l)))
    }
  lazy val longFactor: PackratParser[Typename] = (
      shortFactor~"^"~wholeNumber ^^ { case t~"^"~n => power(t, n.toInt) }
    | shortFactor
    )
  lazy val shortFactor: PackratParser[Typename] = (
      unitname ^^ { case u => SimpleType(u) }
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
    case Success(r, _) => r
  }

  // grammar
  def typename: Parser[Typename] = (
      id~"["~typename~","~typename~"]" ^^ {
        case "Times"~"["~t~","~u~"]" => Times(t, u)
        case "Divide"~"["~t~","~u~"]" => Divide(t, u)
      }
    | id~"["~typename~"]" ^^ { case n~"["~t~"]" => GenericType(n, t) }
    | id ^^ (n => new SimpleType(n))
    )

  def id: Parser[String] = rep1sep(ident, ".") ^^ ( _.last )
}

object MeasuredNumberImpl {

  def u(nEx: Int, unitEx: String) = macro u_impl

  def u_impl(c: Context)(nEx: c.Expr[Int], unitEx: c.Expr[String]): c.Expr[Any] = {
    import c.universe._

    //println(showRaw(c.macroApplication))

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

    val stats = Apply(Select(New(AppliedTypeTree(
      Ident(newTypeName("MeasuredNumber")),
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

    val aID = _precompute(a, typeOf[MeasuredNumber[_]])
    val bID = _precompute(b, typeOf[MeasuredNumber[_]])
    (evals, aID, bID)
  }

  def parseType(c: Context)(tpe: c.Type) = TypeParser.parse(tpe.toString.replace("$", "")) match {
      case GenericType(_, param) => reduce(param.simplify.sortBy(_.toString))
    }


  def addition_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasuredNumber[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {

    import c.universe._

    println(tag.actualType.toString)
    val typeA = parseType(c)(tag.actualType)
    val typeB = parseType(c)(that.actualType)

    if(typeA != typeB)
      c.abort(c.enclosingPosition, s"type error, $typeA != $typeB")

    val resultType = typeA.toTree(c)

    println(s"result type is $resultType")

    val (evals, aID, bID) = precompute(c)(that.tree, c.prefix.tree)

    val stats = Apply(Select(New(AppliedTypeTree(
      Ident(newTypeName("MeasuredNumber")),
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
    (that: c.Expr[MeasuredNumber[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {

    import c.universe._

    val typeA = parseType(c)(tag.actualType)
    val typeB = parseType(c)(that.actualType)

    val resultType = reduce(Times(typeA, typeB).simplify).toTree(c)


    val (evals, aID, bID) = precompute(c)(that.tree, c.prefix.tree)

    val stats = Apply(Select(New(AppliedTypeTree(
      Ident(newTypeName("MeasuredNumber")),
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
