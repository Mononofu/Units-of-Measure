package macroimpl

import language.experimental.macros
import scala.reflect.macros.Context
import collection.mutable.ListBuffer
import scala.reflect.runtime.universe.{WeakTypeTag, TypeTag, Type}

object Helpers {
  val packageName = "$$units$$"

  def simplify(units: Seq[GeneralUnit]): Seq[GeneralUnit] = {
    def reduceUnits(a: GeneralUnit, b: GeneralUnit) = SUnit(a.name, a.power + b.power)

    val unitGroups = units.groupBy(_.name).toList
    val unitList = for((_, units) <- unitGroups) yield units.reduce(reduceUnits)
    unitList.filter(_.power != 0).sortBy(_.name)
  }

  def combine(units: Seq[GeneralUnit]): GeneralUnit = {
    def makeTypes(next: GeneralUnit, sum: GeneralUnit) = CUnit(next, sum)
    val unitList = simplify(units)
    unitList match {
      case unit :: Nil => unit
      case xs => xs.reduceRight(makeTypes)
    }
  }

  def extractConstant[C <: Context, T](c: C)(s: c.universe.Tree): T = s match {
    case c.universe.Literal(c.universe.Constant(longName)) => longName match {
      case s: T => s
    }
  }

  def lookupShortUnit[C <: Context](c: C, short: String): String = {
    if(short == "1") return "Unit"
    val unitSymbol = c.mirror.staticClass(packageName + ".Translate$" + short)
    val dummy = unitSymbol.typeSignature
    val annotations = unitSymbol.asClass.annotations
    annotations.find(a => a.tpe == c.universe.typeOf[LongName]) match {
      case None => c.abort(c.enclosingPosition, s"unknown unit '$short'")
      case Some(a) => extractConstant(c)(a.scalaArgs.head)
    }
  }

  def lookupLongUnit[C <: Context](c: C, long: String): String = {
    if(long == "Unit") return "1"
    val unitSymbol = c.mirror.staticClass(packageName + ".Translate$" + long)
    val dummy = unitSymbol.typeSignature
    val annotations = unitSymbol.asClass.annotations
    annotations.find(a => a.tpe == c.universe.typeOf[ShortName]) match {
      case None => c.abort(c.enclosingPosition, s"unknown unit '$long'")
      case Some(a) => extractConstant(c)(a.scalaArgs.head)
    }
  }

  def lookupBaseUnit[C <: Context](c: C, unitName: String): Option[(String, Double, Double)] = {
    if(unitName == "1" || unitName == "Unit") return None
    val unitSymbol = c.mirror.staticClass(packageName + ".Translate$" + unitName)
    val dummy = unitSymbol.typeSignature
    val annotations = unitSymbol.asClass.annotations
    annotations.find(a => a.tpe == c.universe.typeOf[BaseUnit]).map {
      case a => (extractConstant(c)(a.scalaArgs.head),
                 extractConstant(c)(a.scalaArgs(1)),
                 extractConstant(c)(a.scalaArgs(2)))
    }
  }

  def parseType(c: Context)(tpe: c.Type) = TypeParser.parse(tpe.toString.replace("$", "").replace("macroimpl.", ""))

  def enforceUnitEquality(c: Context)(tpeA: c.Type, tpeB: c.Type) = {
    val typeA = parseType(c)(tpeA)
    val typeB = parseType(c)(tpeB)

    if(typeA != typeB)
      c.abort(c.enclosingPosition, s"type error, $typeA != $typeB")

    combine(typeA).toTree(c)
  }
}

import Helpers._


object MeasureImpl {
  def u(nEx: Int, unitEx: String) = macro u_int_impl
  def u(nEx: Long, unitEx: String) = macro u_long_impl
  def u(nEx: Float, unitEx: String) = macro u_float_impl
  def u(nEx: Double, unitEx: String) = macro u_double_impl
  def u[T: Numeric](nEx: T, unitEx: String)(implicit tag: WeakTypeTag[T]) = macro u_numeric_impl[T]

  type u(unitEx: String, tpeEx: Type) = macro u_unit_impl
  type u_i(unitEx: String) = macro u_unit_int_impl
  type u_l(unitEx: String) = macro u_unit_long_impl
  type u_f(unitEx: String) = macro u_unit_float_impl
  type u_d(unitEx: String) = macro u_unit_double_impl

  def compute_unit(c: Context)(unitEx: c.Expr[String]) = {
    import c.universe._

    val unit = unitEx match {
      case Expr(Literal(Constant(s))) => s.toString
      case _ => c.abort(c.enclosingPosition, "unit has to be a constant string")
    }

    UnitParser[c.type](c).parse(unit)
  }

  def u_int_impl(c: Context)
        (nEx: c.Expr[Int], unitEx: c.Expr[String]): c.Expr[Any] = {
    import c.universe._
    val comp = new Precomputer[c.type](c)
    val nID = comp.compute(nEx.tree)
    val parsedUnit = compute_unit(c)(unitEx)
    c.Expr(Block(comp.evals.toList, q"new MeasureInt[$parsedUnit]($nID)"))
  }

  def u_long_impl(c: Context)
        (nEx: c.Expr[Long], unitEx: c.Expr[String]): c.Expr[Any] = {
    import c.universe._
    val comp = new Precomputer[c.type](c)
    val nID = comp.compute(nEx.tree)
    val parsedUnit = compute_unit(c)(unitEx)
    c.Expr(Block(comp.evals.toList, q"new MeasureLong[$parsedUnit]($nID)"))
  }

  def u_float_impl(c: Context)
        (nEx: c.Expr[Float], unitEx: c.Expr[String]): c.Expr[Any] = {
    import c.universe._
    val comp = new Precomputer[c.type](c)
    val nID = comp.compute(nEx.tree)
    val parsedUnit = compute_unit(c)(unitEx)
    c.Expr(Block(comp.evals.toList, q"new MeasureFloat[$parsedUnit]($nID)"))
  }

  def u_double_impl(c: Context)
        (nEx: c.Expr[Double], unitEx: c.Expr[String]): c.Expr[Any] = {
    import c.universe._
    val comp = new Precomputer[c.type](c)
    val nID = comp.compute(nEx.tree)
    val parsedUnit = compute_unit(c)(unitEx)
    c.Expr(Block(comp.evals.toList, q"new MeasureDouble[$parsedUnit]($nID)"))
  }

  def u_numeric_impl[T: c.WeakTypeTag](c: Context)
        (nEx: c.Expr[T], unitEx: c.Expr[String])
        (evidence$1: c.Expr[Numeric[T]], tag: c.Expr[WeakTypeTag[T]]): c.Expr[Any] = {
    import c.universe._
    val comp = new Precomputer[c.type](c)
    val nID = comp.compute(nEx.tree)
    val parsedUnit = compute_unit(c)(unitEx)
    val typeParm = tag.actualType match {
      case TypeRef(_, _, List(TypeRef(_, t, _))) => t
    }
    c.Expr(Block(comp.evals.toList, q"new Measure[$parsedUnit, $typeParm]($nID)"))
  }



  def u_unit_impl(c: Context)(unitEx: c.Expr[String], tpeEx: c.Expr[Type]): c.Tree = {
    val parsedUnit = compute_unit(c)(unitEx)
    import c.universe._
    AppliedTypeTree( Ident(newTypeName("Measure")), List( parsedUnit ) )
  }

  def u_unit_int_impl(c: Context)(unitEx: c.Expr[String]): c.Tree = {
    val parsedUnit = compute_unit(c)(unitEx)
    import c.universe._
    AppliedTypeTree( Ident(newTypeName("MeasureInt")), List( parsedUnit ) )
  }

  def u_unit_long_impl(c: Context)(unitEx: c.Expr[String]): c.Tree = {
    val parsedUnit = compute_unit(c)(unitEx)
    import c.universe._
    AppliedTypeTree( Ident(newTypeName("MeasureLong")), List( parsedUnit ) )
  }

  def u_unit_float_impl(c: Context)(unitEx: c.Expr[String]): c.Tree = {
    val parsedUnit = compute_unit(c)(unitEx)
    import c.universe._
    AppliedTypeTree( Ident(newTypeName("MeasureFloat")), List( parsedUnit ) )
  }

  def u_unit_double_impl(c: Context)(unitEx: c.Expr[String]): c.Tree = {
    val parsedUnit = compute_unit(c)(unitEx)
    import c.universe._
    AppliedTypeTree( Ident(newTypeName("MeasureDouble")), List( parsedUnit ) )
  }





  def as_impl[T: c.WeakTypeTag]
    (c: Context)(unitEx: c.Expr[String])(tag: c.Expr[WeakTypeTag[T]]): c.Expr[Any] = {
    val parsedUnit = compute_unit(c)(unitEx)
    import c.universe._

    val typeSource = parseType(c)(tag.actualType)
    val treeTarget = compute_unit(c)(unitEx)
    val typeTarget = TypeParser.parse(treeTarget.toString.replace("$", ""))


    // purge units that occur in both source and target
    var leftoverUnits: List[GeneralUnit] = List()
    var targetUnits = typeTarget.toList
    for(u <- typeSource) {
      targetUnits.find(_.name == u.name) match {
        case None => leftoverUnits ::= u
        case Some(v) =>
          targetUnits = targetUnits.filter(_.name != u.name)
          if(v.power - u.power != 0) {
            targetUnits ::= SUnit(v.name, v.power - u.power)
          }
      }
    }

    var targetUnitsBase = ListBuffer[GeneralUnit]()
    var sourceUnitsBase = ListBuffer[GeneralUnit]()

    def getBase(unitEx: String, combineOffsets: (Double, Double, Double) => Double):
      (Seq[GeneralUnit], Double, Double) = {
      val base: Seq[(Seq[GeneralUnit], Double, Double)] = UnitParser(c).parseToUnitList(unitEx).map {
        case long => lookupBaseUnit(c, long.name) match {
          case None => (List(long), 1.0, 0.0)
          case Some((shortBaseEx, newFactor, newOffset)) =>
            val (units, factor, offset) = getBase(shortBaseEx, combineOffsets)
            val correctedUnits = units.map(u => SUnit(u.name, u.power * long.power))
            (correctedUnits,
             Math.pow(factor, long.power) * newFactor,
             combineOffsets(newOffset, offset, newFactor))
        }
      }
      val factor = base.map(_._2).reduce(_ * _)
      val units = base.flatMap(_._1)
      val offset = base.map(_._3).reduce(_ + _)
      (units, factor, offset)
    }

    def combineOffsetTargetUnits(oldOffset: Double, newOffset: Double, factor: Double) = {
      newOffset / factor - oldOffset
    }

    def combineOffsetSourceUnits(oldOffset: Double, newOffset: Double, factor: Double) = {
      oldOffset * factor + newOffset
    }

    def processUnits(src: Seq[GeneralUnit], dst: ListBuffer[GeneralUnit],
      baseOffOp: (Double, Double, Double) => Double): (Double, Double) = {
      var totalFactor = 1.0
      var totalOffset = 0.0
      for(u <- src) {
        val short = lookupLongUnit(c, u.name)
        val (baseUnits, factor, offset) = getBase(s"$short^${u.power}", baseOffOp)
        baseUnits.foreach(v => dst += v)
        totalOffset += offset
        totalFactor *= factor
      }
      (totalFactor, totalOffset)
    }

    val (targetFactor, targetOffset) = processUnits(targetUnits, targetUnitsBase,
      combineOffsetTargetUnits)
    val (sourceFactor, sourceOffset) = processUnits(leftoverUnits, sourceUnitsBase,
      combineOffsetSourceUnits)

    if(simplify(sourceUnitsBase) != simplify(targetUnitsBase)) {
      c.abort(c.enclosingPosition, s"couldn't convert Measure - incompatible units: $sourceUnitsBase, $targetUnitsBase")
    }

    val comp = new Precomputer[c.type](c)
    val nID = comp.compute(c.prefix.tree)

    val fullType = c.prefix.tree.tpe.toString
    val baseType = fullType.substring(0, fullType.indexOf("["))
    val className = baseType.substring(baseType.lastIndexOf(".") + 1)

    val valueExp = q"($nID.n.toDouble * $sourceFactor + $sourceOffset) / $targetFactor + $targetOffset"
    val stats = className match {
      case "MeasureInt" => q"new MeasureInt[$parsedUnit](($valueExp).toInt)"
      case "MeasureLong" => q"new MeasureLong[$parsedUnit](($valueExp).toLong)"
      case "MeasureFloat" => q"new MeasureFloat[$parsedUnit](($valueExp).toFloat)"
      case "MeasureDouble" => q"new MeasureDouble[$parsedUnit]($valueExp)"
      case "Measure" => q"new Measure[$parsedUnit]($valueExp)"
    }

    c.Expr(Block(comp.evals.toList, stats))
  }

  def get_unit_impl[T: c.WeakTypeTag](c: Context)
    (tag: c.Expr[WeakTypeTag[T]]): c.Expr[String] = {

    def prettyUnit(u: GeneralUnit) = {
      val short = lookupLongUnit(c, u.name)
      if(u.power.abs == 1) short
      else s"$short^${Math.abs(u.power)}"
    }

    val typeA = parseType(c)(tag.actualType)
    val (posUnits, negUnits) = simplify(typeA).partition(u => u.power > 0)
    val posUnitsString = posUnits match {
      case Nil => "1"
      case l => l.map(prettyUnit).mkString("*")
    }
    val negUnitsString = negUnits match {
      case Nil => ""
      case l => " / " + negUnits.map(prettyUnit).mkString
    }
    val unit = posUnitsString + negUnitsString

    import c.universe._
    c.Expr[String](Literal(Constant(unit)))
  }
}
