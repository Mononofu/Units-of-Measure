import language.experimental.macros
import scala.reflect.macros.Context
import collection.mutable.ListBuffer
import scala.reflect.runtime.universe.{WeakTypeTag, TypeRef, TypeTag}


object Helpers {
  def reduce(l: List[Typename]): List[Typename] = l match {
    case x :: xs => xs.indexOf(Inverted(x).reduce) match {
      case -1 => x :: reduce(xs)
      case n  => reduce(xs.slice(0, n) ++ xs.slice(n + 1, xs.length))
    }
    case Nil => Nil
  }
}

import Helpers._

class MeasuredNumber[T: WeakTypeTag](val n: Int) {
  override def toString = paramInfo

  def paramInfo(implicit tag: WeakTypeTag[MeasuredNumber[T]]) = {
    val targs = tag.tpe match { case TypeRef(_, _, args) => args }
    val units = TypeParser.parse(targs(0).toString)
    val (a, b) = reduce(units.simplify).partition(x => x match {
      case Inverted(_) => false
      case _ => true
      })
    val punits = a.mkString(" * ") + (b match {
      case Nil => ""
      case l => " / " + b.mkString(" * ")
    })
    s"$n ${units.toString}, raw: ${targs(0)} - ${units.simplify} - $punits"
  }

  def +[U](that: MeasuredNumber[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasuredNumberImpl.addition_impl[T, U]

  def *[U](that: MeasuredNumber[U])(implicit tag: WeakTypeTag[T], tag2: WeakTypeTag[U]) =
    macro MeasuredNumberImpl.multiplication_impl[T, U]
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
}

abstract class Typename {
  def simplify: List[Typename] = List()
  def toTree(c: Context): c.universe.TypTree = throw new Exception("invalid state")
}

case class SimpleType(name: String) extends Typename {
  override def toString = name
  override def simplify = List(this)
  override def equals(that: Any) = that match {
    case SimpleType(n) => n == name
    case _ => false
  }
  override def toTree(c: Context) = c.universe.Ident(c.universe.newTypeName(name))
}
case class GenericType(name: String, param: Typename) extends Typename
case class GenericType2(name: String, param1: Typename, param2: Typename) extends Typename

case class Times[T, U](param1: Typename, param2: Typename) extends Typename {
  override def toString = s"$param1 * $param2"
  override def simplify = param1.simplify ++ param2.simplify
  override def toTree(c: Context) = c.universe.AppliedTypeTree(
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
  override def toTree(c: Context) = c.universe.AppliedTypeTree(
          c.universe.Ident(c.universe.newTypeName("Divide")),
          List(param1.toTree(c), param2.toTree(c)))
}



import scala.util.parsing.combinator._

object UnitParser extends JavaTokenParsers {
  def parse(in: String, c: Context) = parseAll(unit, in) match {
    case Success(r, _) => r.toTree(c)
  }

  def toTimes(units: List[String]): Typename = units match {
    case x :: Nil => SimpleType(x)
    case x :: y :: Nil => Times(SimpleType(x), SimpleType(y))
    case x :: xs => Times(SimpleType(x), toTimes(xs))
  }

  def unit: Parser[Typename] = (
      rep1sep(unitname, "*")~"/"~rep1sep(unitname, "*") ^^ { case times~"/"~divide =>
        Divide(toTimes(times), toTimes(divide)) }
    | rep1sep(unitname, "*") ^^ { case units => toTimes(units)}

    )

  def unitname: Parser[String] = (
      "m" ^^ { _ => "Meter" }
    | "s" ^^ { _ => "Second" }
    | "1"
    )
}

object TypeParser extends JavaTokenParsers {
  def parse(in: String) = parseAll(typename, in) match {
    case Success(r, _) => r
  }

  // grammar
  def typename: Parser[Typename] = (
      "Times["~typename~","~typename~"]" ^^ { case "Times["~t~","~u~"]" => Times(t, u) }
    | "Divide["~typename~","~typename~"]" ^^ { case "Divide["~t~","~u~"]" => Divide(t, u) }
    | ident~"["~typename~","~typename~"]" ^^ { case n~"["~t~","~u~"]" => GenericType2(n, t, u) }
    | ident~"["~typename~"]" ^^ { case n~"["~t~"]" => GenericType(n, t) }
    | ident ^^ (n => new SimpleType(n))
    )
}

object MeasuredNumberImpl {

  def u(nEx: Int, unitEx: String) = macro u_impl

  def u_impl(c: Context)(nEx: c.Expr[Int], unitEx: c.Expr[String]): c.Expr[Any] = {
    import c.universe._

    val evals = ListBuffer[ValDef]()

    def _precompute(value: Tree, tpe: Type): Ident = {
      val freshName = newTermName(c.fresh("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }

    val nID = _precompute(nEx.tree, typeOf[Int])
    //val unitID = _precompute(unitEx.tree, typeOf[String])

    val unit = unitEx match {
      case Expr(Literal(Constant(s))) => s.toString
      case _ => throw new Exception("unit has to be a constant string")
    }

    println(unit)
    val parsedUnit = UnitParser.parse(unit, c)
    println(showRaw(parsedUnit))

    /* new MeasuredNumber[Int](eval$17)(Predef.this.implicitly)
Apply(Apply(Select(New(AppliedTypeTree(Ident(MeasuredNumber), List(Ident(scala.Int)))), nme.CONSTRUCTOR), List(Ident(newTermName("eval$17")))), List(Select(This(newTypeName("Predef")), newTermName("implicitly"))))
*/

    // val stats = reify(new MeasuredNumber[Times[Int, Double]](c.Expr[Int](nID).splice)).tree
    //val stats = reify(new MeasuredNumber[Int](5)).tree

    /* New(AppliedTypeTree(
      Ident(MeasuredNumber),
      List(
        AppliedTypeTree(
          Ident(Times),
          List(
            Ident(scala.Int),
            Ident(scala.Double))))))*/

    val stats = Apply(Select(New(AppliedTypeTree(
      Ident(newTypeName("MeasuredNumber")),
      List( parsedUnit )
      )), nme.CONSTRUCTOR),
    List(Ident(newTermName(nID.toString))))

    println(stats)
    println(showRaw(stats))

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

  def parseType(c: Context)(tpe: c.Type) = TypeParser.parse(tpe.toString.replace(".", "").replace("$", "")) match {
      case GenericType(_, param) => reduce(param.simplify.sortBy(_.toString)).toList
    }


  def addition_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasuredNumber[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {

    import c.universe._

    val typeA = parseType(c)(tag.actualType)
    val typeB = parseType(c)(that.actualType)

    println(s"$typeA -- $typeB")

    if(typeA != typeB)
      throw new Exception(s"type error, $typeA != $typeB")

    val (evals, a, b) = precompute(c)(that.tree, c.prefix.tree)

    val stats = reify(new MeasuredNumber[U](c.Expr[MeasuredNumber[T]](a).splice.n +
      c.Expr[MeasuredNumber[U]](b).splice.n)).tree

    /*println()
    println()
    println(stats)
    println()
    println(showRaw(stats))
    println()
    println()*/

    c.Expr(Block(evals.toList, stats))
  }

  def multiplication_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasuredNumber[U]])
    (tag: c.Expr[WeakTypeTag[T]], tag2: c.Expr[WeakTypeTag[U]]): c.Expr[Any] = {

    import c.universe._

    val typeA = parseType(c)(tag.actualType)
    val typeB = parseType(c)(that.actualType)

    println(s"$typeA -- $typeB")

    val (evals, a, b) = precompute(c)(that.tree, c.prefix.tree)

    val stats = reify(new MeasuredNumber[Times[T, U]](c.Expr[MeasuredNumber[T]](a).splice.n *
      c.Expr[MeasuredNumber[U]](b).splice.n)).tree
    c.Expr(Block(evals.toList, stats))
  }
}
