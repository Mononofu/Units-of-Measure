package units

import macroimpl._
import CreateUnitMacros.NewUnit

// SI Base Units
trait Meter extends NewUnit("m")
trait Kilogram extends NewUnit("kg", (1000, "g"))
trait Second extends NewUnit("s")
trait Ampere extends NewUnit("A")
trait Kelvin extends NewUnit("K")
trait Mole extends NewUnit("mol")
trait Candela extends NewUnit("cd")

// derived units
trait Gram extends NewUnit("g")

trait Kilometer extends NewUnit("km", (1000, "m"))
trait Centimeter extends NewUnit("cm", (0.01, "m"))

trait Minute extends NewUnit("min", (60, "s"))
trait Hour extends NewUnit("h", (60, "min"))

trait Foot extends NewUnit("ft", (0.3048, "m"))

trait Newton extends NewUnit("N", (1, "kg*m/s^2"))
trait Dyne extends NewUnit("dyn", (1, "g*cm/s^2"))

trait Radian extends NewUnit("rad", (1, "1"))
trait Degree extends NewUnit("deg", (Math.PI/180., "rad"))

trait Celsius extends NewUnit("C", (1, "K"), 273.15)
trait Fahrenheit extends NewUnit("F", (5.0/9.0, "C"), -32.0)

trait Volt extends NewUnit("V", (1, "kg*m^2 / (A*s^3)"))
trait Ohm extends NewUnit("ohm", (1, "kg*m^2 / (A^2*s^3)"))
trait Watt extends NewUnit("W", (1, "kg m^2 s^-3"))

trait Joule extends NewUnit("J", (1, "kg m^2 s^-2"))
