package macroimpl

import language.experimental.macros
import scala.reflect.macros.Context

case class LongName(n: String) extends scala.annotation.StaticAnnotation
case class ShortName(n: String) extends scala.annotation.StaticAnnotation
case class BaseUnit(name: String, factor: Double, offset: Double) extends scala.annotation.StaticAnnotation

@LongName("MyUnit") class TranslateF

object CreateUnitMacros {

  def extractString[C <: Context](c: C)(e: c.Expr[String]) = {
    import c.universe._
    e match {
      case Expr(Literal(Constant(s))) => s.toString
      case _ => c.abort(c.enclosingPosition, "unit name has to be a constant string")
    }
  }

  import Helpers.packageName

  def createImpl(c: Context)(long: c.Expr[String], short: c.Expr[String]): c.Tree = {
    import c.universe._
    val Template(_, _, existingCode) = c.enclosingTemplate

    val longName = extractString[c.type](c)(long)
    val shortName = extractString[c.type](c)(short)

    val classNameShort = newTypeName(s"Translate$$$shortName")
    val unitLookupShort = q"@macroimpl.LongName(n = $longName) class $classNameShort"
    c.introduceTopLevel(packageName, unitLookupShort)

    val classNameLong = newTypeName(s"Translate$$$longName")
    val unitLookupLong = q"@macroimpl.ShortName(n = $shortName) class $classNameLong"
    c.introduceTopLevel(packageName, unitLookupLong)

    Template(Nil, emptyValDef, existingCode )
  }

  def createImplConv(c: Context)
    (long: c.Expr[String], short: c.Expr[String], base: c.Expr[(Double, String)]): c.Tree = {
    import c.universe._
    createImplConvOff(c)(long, short, base, c.Expr(Literal(Constant(0.0))))
  }


  def createImplConvOff(c: Context)
    (long: c.Expr[String], short: c.Expr[String],
      base: c.Expr[(Double, String)], offsetEx: c.Expr[Double]): c.Tree = {
    import c.universe._
    val Template(_, _, existingCode) = c.enclosingTemplate

    val longName = extractString[c.type](c)(long)
    val shortName = extractString[c.type](c)(short)
    val (baseName: String, factor: Double) = base match {
      case Expr(Apply(_, List(Literal(Constant(f: Double)), Literal(Constant(n))))) => (n.toString, f.toDouble)
      case _ => c.abort(c.enclosingPosition, "base unit has to have format: (<number>, <unit>). (100, \"m\")")
    }
    val offset = offsetEx match {
      case Expr(Literal(Constant(s: Double))) => s
      case _ => c.abort(c.enclosingPosition, "offset has to be a constant value")
    }

    val classNameShort = newTypeName(s"Translate$$$shortName")
    val unitLookupShort = q"@macroimpl.LongName(n = $longName) @macroimpl.BaseUnit(name = $baseName, factor = $factor, offset = $offset) class $classNameShort"
    c.introduceTopLevel(packageName, unitLookupShort)

    val classNameLong = newTypeName(s"Translate$$$longName")
    val unitLookupLong = q"@macroimpl.ShortName(n = $shortName) @macroimpl.BaseUnit(name = $baseName, factor = $factor, offset = $offset) class $classNameLong"
    c.introduceTopLevel(packageName, unitLookupLong)

    Template(Nil, emptyValDef, existingCode )
  }

  type MyUnit(long: String, short: String) = macro createImpl

  type MyUnit(long: String, short: String, base: (Double, String)) =
    macro createImplConv

  type MyUnit(long: String, short: String, base: (Double, String), offsetEx: Double) =
    macro createImplConvOff
}
