import language.experimental.macros
import scala.reflect.macros.Context
import collection.mutable.ListBuffer
import scala.reflect.runtime.universe.{WeakTypeTag, TypeRef, TypeTag}

abstract class Measure
abstract class Meter extends Measure
abstract class Second extends Measure

abstract class Divide[N, M] extends Measure
abstract class Times[N, M] extends Measure

case class Number[T: WeakTypeTag](n: Int) {
  override def toString = paramInfo

  def paramInfo(implicit tag: WeakTypeTag[Number[T]]) = {
    val targs = tag.tpe match { case TypeRef(_, _, args) => args }
    s"type of $n has type arguments $targs"
  }
}

object NumberImpl {
  def addition_impl[T: c.WeakTypeTag](c: Context)
    (n: c.Expr[Number[T]], that: c.Expr[Number[T]]):
      c.Expr[Number[T]] = {




    import c.universe._

    val evals = ListBuffer[ValDef]()
    def precompute(value: Tree, tpe: Type): Ident = {
      val freshName = newTermName(c.fresh("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }

    val a = precompute(n.tree, typeOf[Number[_]])
    val b = precompute(that.tree, typeOf[Number[_]])

    val stats = reify(Number[T](c.Expr[Number[T]](a).splice.n +
      c.Expr[Number[T]](b).splice.n)).tree
    println(showRaw(stats))
    c.Expr[Number[T]](Block(evals.toList, stats))
  }

  def multiplication_impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
    (c: Context)(n: c.Expr[Number[T]], that: c.Expr[Number[U]])
    (evidence$2: c.Expr[WeakTypeTag[U]]):
      c.Expr[Any] = {

    import c.universe._

    println(that.actualType)

    val evals = ListBuffer[ValDef]()
    def precompute(value: Tree, tpe: Type): Ident = {
      val freshName = newTermName(c.fresh("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }

    val a = precompute(n.tree, typeOf[Number[_]])
    val b = precompute(that.tree, typeOf[Number[_]])

    val stats = reify(Number[Times[T, U]](c.Expr[Number[T]](a).splice.n *
      c.Expr[Number[T]](b).splice.n)).tree
    c.Expr[Number[Times[T, U]]](Block(evals.toList, stats))
  }
}
