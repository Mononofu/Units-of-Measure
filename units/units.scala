package units

import macroimpl._
import CreateUnitMacros.MyUnit

trait Meter extends MyUnit("Meter", "m")

trait Second extends MyUnit("Second", "s")
trait Minute extends MyUnit("Minute", "min", (60, "s"))
trait Hour extends MyUnit("Hour", "h", (60, "min"))

trait Foot extends MyUnit("Foot", "ft", (0.3048, "m"))
trait Kilometer extends MyUnit("Kilometer", "km", (1000, "m"))

