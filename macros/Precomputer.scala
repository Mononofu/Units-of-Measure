package macroimpl

import collection.mutable.ListBuffer
import scala.reflect.macros.Context

class Precomputer[C <: Context](val c: C) {
  import c.universe._
  val evals = ListBuffer[ValDef]()

  def compute(value: c.Tree): Ident = {
    val freshName = newTermName(c.fresh("eval$"))
    evals += ValDef(Modifiers(), freshName, TypeTree(value.tpe), value)
    Ident(freshName)
  }
}
