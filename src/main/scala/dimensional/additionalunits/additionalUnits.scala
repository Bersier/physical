package dimensional.additionalunits

import dimensional.dimension.Dimensions.{*, given}
import dimensional.typelevelint.*

import scala.language.implicitConversions

val degree: Angle = math.Pi / 180 * radian

val minute    : Time = 60 * second
val hour      : Time = 60 * minute
val day       : Time = 24 * hour
val julianYear: Time = 365.25 * day

val electronVolt: Energy = 1.602176634e-19 * joule
val smallCalorie: Energy = 4.184 * joule
val largeCalorie: Energy = kilo(smallCalorie)
val tonOfTNT    : Energy = mega(largeCalorie)
val wattHour    : Energy = watt * hour
val kiloWattHour: Energy = kilo(wattHour)

val shannon: Information = math.log(2) * nat
val bit    : Information = shannon
val byte   : Information = 8 * bit

val kPH        : Velocity = kilo(metre) / hour
val planckSpeed: Velocity = 299792458 * metre / second

val angstrom    : Length = 1e-10 * metre
val meter       : Length = metre
val inch        : Length = 0.0254 * metre
val foot        : Length = 12 * inch
val yard        : Length = 3 * foot
val furlong     : Length = 220 * yard
val mile        : Length = 8 * furlong
val nauticalMile: Length = 1852 * metre
val lightYear   : Length = planckSpeed * julianYear

val knot: Velocity = nauticalMile / hour

val gram     : Mass = milli(kilogram)
val poundMass: Mass = 0.45359237 * kilogram
val stone    : Mass = 14 * poundMass
val shortTon : Mass = 2000 * poundMass
val tonne    : Mass = kilo(kilogram)

val atmosphere: Pressure = 101325 * pascal
val bar       : Pressure = 1e5 * pascal
val pSI       : Pressure = 6894.75729317 * pascal

val centipoise: Pressure * Time = 1e-3 * pascal * second

val litre      : Volume = 1e-3 * metre ~ _3
val liter      : Volume = litre
val fluidOunce : Volume = 29.5735295625 * milli(litre)
val liquidPint : Volume = 16 * fluidOunce
val liquidQuart: Volume = 2 * liquidPint
val usGallon   : Volume = 4 * liquidQuart
