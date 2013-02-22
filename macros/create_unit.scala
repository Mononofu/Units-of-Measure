package macroimpl

import language.experimental.macros
import scala.reflect.macros.Context


object CreateUnitMacros {

  def createImpl(c: Context)(name: c.Expr[String], short: c.Expr[String]): c.Expr[Any] = {
    import c.universe._

    val className = name match {
      case Expr(Literal(Constant(s))) => c.universe.newTypeName(s.toString)
      case _ => c.abort(c.enclosingPosition, "unit name has to be a constant string")
    }

    val packageName = c.enclosingPackage.pid.toString
    c.introduceTopLevel(packageName, q"class $className")

    c.Expr(Block(List(), Literal(Constant())))
  }

  def createUnit(name: String, short: String) = macro createImpl
}
