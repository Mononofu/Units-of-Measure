package units

import macroimpl._
import CreateUnitMacros.MyUnit

object Units {
  trait Meter extends MyUnit("Meter", "m")
  trait Second extends MyUnit("Second", "s")
}
