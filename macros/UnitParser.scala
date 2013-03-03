package macroimpl

import scala.util.parsing.combinator._
import scala.reflect.macros.Context

import Helpers._

case class UnitParser[C <: Context](c: C) extends JavaTokenParsers with PackratParsers {
  def parse(in: String) = parseAll(term, in) match {
    case Success(r, _) => combine(r).toTree(c)
    case _ => c.abort(c.enclosingPosition, s"unknown units and/or invalid format '$in'")
  }

  /* returns a list of SUnits */
  def parseToUnitList(in: String) = parseAll(term, in) match {
    case Success(r, _) => simplify(r)
    case _ => c.abort(c.enclosingPosition, s"unknown units and/or invalid format '$in'")
  }

  def toTypenames(units: List[~[String, List[GeneralUnit]]]): List[GeneralUnit] = units match {
    case Nil => Nil
    case ("*"~x) :: xs => x ++ toTypenames(xs)
    case (""~x) :: xs => x ++ toTypenames(xs)
    case ("/"~x) :: xs => x.map(_.invert) ++ toTypenames(xs)
  }

  def power(t: List[GeneralUnit], n: Int): List[GeneralUnit] = n match {
    case 1 => t
    case 0 => List(SUnit("Unit"))
    case -1 => t.map(_.invert)
    case n if n > 0 => t ++ power(t, n - 1)
    case _ => t.map(_.invert) ++ power(t, n + 1)
  }

  lazy val term: PackratParser[List[GeneralUnit]] =
    longFactor~rep(("*"|"/"|"")~longFactor) ^^ {
      case t~Nil => t
      case t~l => t ++ toTypenames(l)
    }
  lazy val longFactor: PackratParser[List[GeneralUnit]] = (
      shortFactor~"^"~wholeNumber ^^ { case t~"^"~n => power(t, n.toInt) }
    | shortFactor
    )
  lazy val shortFactor: PackratParser[List[GeneralUnit]] = (
      unitname ^^ { case u => List(SUnit(u)) }
    | "("~>term<~")"
    )

  lazy val unitname: PackratParser[String] = (
      "1" ^^ { _ => "Unit" }
    | "[a-zA-Z]+".r ^^ { short =>
        lookupShortUnit(c, short)
      }
    )
}
