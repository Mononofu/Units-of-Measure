package macroimpl

import scala.util.parsing.combinator._

import Helpers._

object TypeParser extends JavaTokenParsers {
  def parse(in: String) = parseAll(typename, in) match {
    case Success(r, _) => simplify(r)
  }

  def dimValue(dim: String) = dim match {
    case "Zero" => 0
    case d if d.startsWith("Pos") => d.substring(3).toInt
    case d => d.substring(3).toInt * -1
  }

  // grammar
  def typename: Parser[List[GeneralUnit]] = (
      "CUnit["~typename~","~typename~"]" ^^ {
        case "CUnit["~t~","~u~"]" => t ++ u
      }
    | "SUnit["~id~","~id~"]" ^^ {
      case "SUnit["~u~","~d~"]" => List(SUnit(u, dimValue(d)))
    }
    | id~"["~typename~"]" ^^ { case n~"["~t~"]" => t }
    )

  def id: Parser[String] = rep1sep(ident, ".") ^^ ( _.last )
}
