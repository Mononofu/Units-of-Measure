package units

import macroimpl._
import CreateUnitMacros.MyUnit

trait Meter extends MyUnit("Meter", "m")
trait Kilometer extends MyUnit("Kilometer", "km", (1000, "m"))
trait Centimeter extends MyUnit("Centimeter", "cm", (0.01, "m"))

trait Second extends MyUnit("Second", "s")
trait Minute extends MyUnit("Minute", "min", (60, "s"))
trait Hour extends MyUnit("Hour", "h", (60, "min"))

trait Foot extends MyUnit("Foot", "ft", (0.3048, "m"))

trait Gram extends MyUnit("Gram", "g")
trait Kilogram extends MyUnit("Kilogram", "kg", (1000, "g"))

trait Newton extends MyUnit("Newton", "N", (1, "kg*m/s^2"))
trait Dyne extends MyUnit("Dyne", "dyn", (1, "g*cm/s^2"))

trait Radian extends MyUnit("Radian", "rad", (1, "1"))
trait Degree extends MyUnit("Degree", "deg", (3.1416/180., "rad"))

trait Kelvin extends MyUnit("Kelvin", "K")
trait Celsius extends MyUnit("Celsius", "C", (1, "K"), 273.15)
trait Fahrenheit extends MyUnit("Fahrenheit", "F", (5.0/9.0, "C"), -32.0)
