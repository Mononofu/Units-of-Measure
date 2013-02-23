package macroimpl

import language.experimental.macros
import scala.reflect.macros.Context


object CreateUnitMacros {

  def createImpl(c: Context)(long: c.Expr[String], short: c.Expr[String]): c.Tree = {
    import c.universe._

    def extractString(e: c.Expr[String]) = e match {
      case Expr(Literal(Constant(s))) => s.toString
      case _ => c.abort(c.enclosingPosition, "unit name has to be a constant string")
    }

    val Template(_, _, existingCode) = c.enclosingTemplate

    val longName = extractString(long)
    val shortName = extractString(short)

    val packageName = c.enclosingPackage.pid.toString
    val className = newTypeName(s"Translate$shortName")
    println(className)
    c.introduceTopLevel(packageName, q"class $className")

    Template(Nil, emptyValDef, existingCode )
  }

  type MyUnit(long: String, short: String) = macro createImpl
}
