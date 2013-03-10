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

  def createImpl(c: Context)(short: c.Expr[String]): c.Tree = {
    import c.universe._
    val Template(_, _, existingCode) = c.enclosingTemplate

    val longName: String = c.enclosingImpl.name.toString
    val shortName = extractString[c.type](c)(short)

    val classNameShort = newTypeName(s"TranslateShort$$$shortName")
    val unitLookupShort = q"@macroimpl.LongName(n = $longName) class $classNameShort"
    c.introduceTopLevel(packageName, unitLookupShort)

    val classNameLong = newTypeName(s"TranslateLong$$$longName")
    val unitLookupLong = q"@macroimpl.ShortName(n = $shortName) class $classNameLong"
    c.introduceTopLevel(packageName, unitLookupLong)

    Template(Nil, emptyValDef, existingCode )
  }

  def createImplConv(c: Context)
    (short: c.Expr[String], base: c.Expr[(Double, String)]): c.Tree = {
    import c.universe._
    createImplConvOff(c)(short, base, c.Expr(Literal(Constant(0.0))))
  }


  def createImplConvOff(c: Context)
    (short: c.Expr[String],
      base: c.Expr[(Double, String)], offsetEx: c.Expr[Double]): c.Tree = {
    import c.universe._
    val Template(_, _, existingCode) = c.enclosingTemplate

    val longName: String = c.enclosingImpl.name.toString
    val shortName = extractString[c.type](c)(short)
    val (baseName: String, factor: Double) = base match {
      case Expr(Apply(_, List(Literal(Constant(f: Double)), Literal(Constant(n))))) => (n.toString, f.toDouble)
      case _ => c.abort(c.enclosingPosition, "base unit has to have format: (<number>, <unit>). (100, \"m\")")
    }
    val offset = offsetEx match {
      case Expr(Literal(Constant(s: Double))) => s
      case _ => c.abort(c.enclosingPosition, "offset has to be a constant value")
    }

    val classNameShort = newTypeName(s"TranslateShort$$$shortName")
    val unitLookupShort = q"@macroimpl.LongName(n = $longName) @macroimpl.BaseUnit(name = $baseName, factor = $factor, offset = $offset) class $classNameShort"
    c.introduceTopLevel(packageName, unitLookupShort)

    val classNameLong = newTypeName(s"TranslateLong$$$longName")
    val unitLookupLong = q"@macroimpl.ShortName(n = $shortName) @macroimpl.BaseUnit(name = $baseName, factor = $factor, offset = $offset) class $classNameLong"
    c.introduceTopLevel(packageName, unitLookupLong)

    Template(Nil, emptyValDef, existingCode )
  }

  type NewUnit(short: String) = macro createImpl

  type NewUnit(short: String, base: (Double, String)) =
    macro createImplConv

  type NewUnit(short: String, base: (Double, String), offsetEx: Double) =
    macro createImplConvOff
}
