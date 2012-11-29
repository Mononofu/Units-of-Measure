import language.experimental.macros
import scala.reflect.macros.Context
import collection.mutable.ListBuffer
import scala.reflect.runtime.universe.{WeakTypeTag, TypeRef, TypeTag}

abstract class Measure
abstract class Meter extends Measure
abstract class Second extends Measure

abstract class Divide[N, M] extends Measure
abstract class Times[N, M] extends Measure

class MeasuredNumber[T: WeakTypeTag](val n: Int) {
  override def toString = paramInfo

  def paramInfo(implicit tag: WeakTypeTag[MeasuredNumber[T]]) = {
    val targs = tag.tpe match { case TypeRef(_, _, args) => args }
    s"type of $n has type arguments $targs"
  }
}

abstract class Typename
case class SimpleType(name: String) extends Typename
case class GenericType(name: String, param: Typename) extends Typename
case class GenericType2(name: String, param1: Typename, param2: Typename) extends Typename

import scala.util.parsing.combinator._

object TypeParser extends JavaTokenParsers {
  def parse(in: String) = parseAll(typename, in) match {
    case Success(r, _) => r
  }

  // grammar
  def typename: Parser[Typename] = (
      ident~"["~typename~","~typename~"]" ^^ { case n~"["~t~","~u~"]" => GenericType2(n, t, u) }
    | ident~"["~typename~"]" ^^ { case n~"["~t~"]" => GenericType(n, t) }
    | ident ^^ (n => SimpleType(n))
    )
}

object MeasuredNumberImpl {
  def addition_impl[T: c.WeakTypeTag](c: Context)
    (that: c.Expr[MeasuredNumber[T]])
    (tag: c.Expr[WeakTypeTag[T]]) = {

    import c.universe._
    
    val typeA = TypeParser.parse(c.prefix.actualType.toString) match {
      case GenericType(_, param) => param
    }

    val typeB = TypeParser.parse(that.actualType.toString) match {
      case GenericType(_, param) => param
    }

    println(s"$typeA -- $typeB")

    if(typeA != typeB)
      throw new Exception(s"type error, $typeA != $typeB")

    val evals = ListBuffer[ValDef]()
    def precompute(value: Tree, tpe: Type): Ident = {
      val freshName = newTermName(c.fresh("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }

    val a = precompute(that.tree, that.actualType)
    val b = precompute(c.prefix.tree, that.actualType)


    //println(c.prefix)

    //println(showRaw(evals(0)))

    /*val stats = Apply(
      Apply(
        TypeApply(Select(Ident(newTypeName("MeasuredNumber")), 
                         newTermName("apply")), 
                  List(TypeTree())), 
        List(
          Apply(Select(Select(Ident(newTermName("eval$1")), 
                              newTermName("n")), 
                      newTermName("$plus")), 
                List(Select(Select(Ident(newTermName("eval$2")),
                              newTermName("num")), 
                              newTermName("n")))))), 
      List(Select(This(newTypeName("Predef")), newTermName("implicitly"))))*/

    val stats = reify(new MeasuredNumber[T](c.Expr[MeasuredNumber[T]](a).splice.n +
      c.Expr[MeasuredNumber[T]](b).splice.n)).tree 
    println(showRaw(stats))
    c.Expr[MeasuredNumber[T]](Block(evals.toList, stats))
  }

  def multiplication_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)
    (that: c.Expr[MeasuredNumber[U]])
    (tag: c.Expr[WeakTypeTag[U]], tag2: c.Expr[WeakTypeTag[T]]) = {

    import c.universe._

    val typeA = TypeParser.parse(c.prefix.actualType.toString) match {
      case GenericType(_, param) => param
    }

    val typeB = TypeParser.parse(that.actualType.toString) match {
      case GenericType(_, param) => param
    }

    println(s"$typeA -- $typeB")

    /*if(typeA != typeB)
      throw new Exception(s"type error, $typeA != $typeB")*/

    //println(that.actualType)

    val evals = ListBuffer[ValDef]()
    def precompute(value: Tree, tpe: Type): Ident = {
      val freshName = newTermName(c.fresh("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }

    val a = precompute(c.prefix.tree, typeOf[MeasuredNumber[_]])
    val b = precompute(that.tree, that.actualType)

    val stats = reify(new MeasuredNumber[Times[T, U]](c.Expr[MeasuredNumber[T]](a).splice.n *
      c.Expr[MeasuredNumber[U]](b).splice.n)).tree
    c.Expr[MeasuredNumber[Times[T, U]]](Block(evals.toList, stats))
  }
}
