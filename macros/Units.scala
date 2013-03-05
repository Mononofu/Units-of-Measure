package macroimpl

import scala.reflect.macros.Context

abstract class Dimension
abstract class Pos1 extends Dimension
abstract class Pos2 extends Dimension
abstract class Pos3 extends Dimension
abstract class Neg1 extends Dimension
abstract class Neg2 extends Dimension
abstract class Neg3 extends Dimension
abstract class Zero extends Dimension

abstract class GeneralUnit {
  def name: String = throw new Exception(s"invalid state, name called on $this")
  def power: Int = throw new Exception(s"invalid state, power called on $this")
  def invert: GeneralUnit = throw new Exception(s"invalid state, invert called on $this")
  def toTree(c: Context): c.universe.Tree =
    throw new Exception(s"invalid state, toTree called on $this")
}
case class SUnit[U, D <: Dimension](override val name: String, override val power: Int = 1) extends GeneralUnit {
  override def toString = power match {
    case 1 => name
    case n => s"$name^$n"
  }
  override def equals(that: Any) = that match {
    case SUnit(n, p) => n == name && p == power
    case _ => false
  }
  def dimName = power match {
    case d if d > 0 => "Pos" + d
    case 0 => "Zero"
    case d => "Neg" + (-d)
  }

  override def toTree(c: Context): c.universe.Tree = c.universe.AppliedTypeTree(
          c.universe.Ident(c.universe.newTypeName("SUnit")),
          List(c.universe.Ident(c.universe.newTypeName(name)),
               c.universe.Ident(c.universe.newTypeName(dimName))))

  override def invert = SUnit(name, -power)
}
case class CUnit[U, V](unit: GeneralUnit, next: GeneralUnit) extends GeneralUnit {
  override def toTree(c: Context): c.universe.Tree = c.universe.AppliedTypeTree(
          c.universe.Ident(c.universe.newTypeName("CUnit")),
          List(unit.toTree(c), next.toTree(c)))
}
