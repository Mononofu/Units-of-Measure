package macroimpl

import language.experimental.macros
import scala.reflect.macros.Context

case class LongName(n: String) extends scala.annotation.StaticAnnotation
case class ShortName(n: String) extends scala.annotation.StaticAnnotation

@LongName("MyUnit") class TranslateF

object CreateUnitMacros {

  def createImpl(c: Context)(long: c.Expr[String], short: c.Expr[String]): c.Tree = {
    import c.universe._
    import Helpers.packageName

    def extractString(e: c.Expr[String]) = e match {
      case Expr(Literal(Constant(s))) => s.toString
      case _ => c.abort(c.enclosingPosition, "unit name has to be a constant string")
    }

    val Template(_, _, existingCode) = c.enclosingTemplate

    val longName = extractString(long)
    val shortName = extractString(short)

    val classNameShort = newTypeName(s"Translate$$$shortName")
    val unitLookupShort = q"@macroimpl.LongName(n = $longName) class $classNameShort"
    c.introduceTopLevel(packageName, unitLookupShort)

    val classNameLong = newTypeName(s"Translate$$$longName")
    val unitLookupLong = q"@macroimpl.ShortName(n = $shortName) class $classNameLong"
    c.introduceTopLevel(packageName, unitLookupLong)

    Template(Nil, emptyValDef, existingCode )
  }

  type MyUnit(long: String, short: String) = macro createImpl
}
