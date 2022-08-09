package dimensional

import dimensional.Dimensions.{*, given}
import dimensional.IntType.*

import scala.language.implicitConversions

/**
 * Provides additional units, as well as some metric and binary prefixes.
 */
object AdditionalUnits:
  val degree: Angle = math.Pi / 180 * radian

  val electronVolt: Energy = 1.602176634e-19 * joule
  val smallCalorie: Energy = 4.184 * joule
  val largeCalorie: Energy = kilo(smallCalorie)
  val tonOfTNT    : Energy = mega(largeCalorie)
  val wattHour    : Energy = watt * hour
  val kiloWattHour: Energy = kilo(wattHour)

  val shannon: Information = math.log(2) * nat
  val bit    : Information = shannon
  val byte   : Information = 8 * bit

  val angstrom    : Length = 1e-10 * metre
  val meter       : Length = metre
  val inch        : Length = 0.0254 * metre
  val foot        : Length = 12 * inch
  val yard        : Length = 3 * foot
  val furlong     : Length = 220 * yard
  val mile        : Length = 8 * furlong
  val nauticalMile: Length = 1852 * metre
  val lightYear   : Length = planckSpeed * julianYear

  val gram     : Mass = milli(kilogram)
  val poundMass: Mass = 0.45359237 * kilogram
  val stone    : Mass = 14 * poundMass
  val shortTon : Mass = 2000 * poundMass
  val tonne    : Mass = kilo(kilogram)

  val atmosphere: Pressure = 101325 * pascal
  val bar       : Pressure = 1e5 * pascal
  val pSI       : Pressure = 6894.75729317 * pascal

  val centipoise: Pressure * Time = 1e-3 * pascal * second

  val minute    : Time = 60 * second
  val hour      : Time = 60 * minute
  val day       : Time = 24 * hour
  val julianYear: Time = 365.25 * day

  val kPH        : Velocity = kilo(metre) / hour
  val planckSpeed: Velocity = 299792458 * metre / second

  val litre      : Volume = 1e-3 * metre ~ _3
  val liter      : Volume = litre
  val fluidOunce : Volume = 29.5735295625 * milli(litre)
  val liquidPint : Volume = 16 * fluidOunce
  val liquidQuart: Volume = 2 * liquidPint
  val usGallon   : Volume = 4 * liquidQuart

  inline def nano[
    L <: IntT,
    T <: IntT,
    P <: IntT,
    M <: IntT,
    Q <: IntT,
    N <: IntT,
    C <: IntT,
    A <: IntT,
    AQ <: IntT,
    AP <: IntT,
    O1 <: IntT,
    O2 <: IntT,
    O3 <: IntT,
    O4 <: IntT,
    O5 <: IntT,
    O6 <: IntT,
  ](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
    L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
  ] = x * 1e-9

  inline def micro[
    L <: IntT,
    T <: IntT,
    P <: IntT,
    M <: IntT,
    Q <: IntT,
    N <: IntT,
    C <: IntT,
    A <: IntT,
    AQ <: IntT,
    AP <: IntT,
    O1 <: IntT,
    O2 <: IntT,
    O3 <: IntT,
    O4 <: IntT,
    O5 <: IntT,
    O6 <: IntT,
  ](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
    L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
  ] = x * 1e-6

  inline def milli[
    L <: IntT,
    T <: IntT,
    P <: IntT,
    M <: IntT,
    Q <: IntT,
    N <: IntT,
    C <: IntT,
    A <: IntT,
    AQ <: IntT,
    AP <: IntT,
    O1 <: IntT,
    O2 <: IntT,
    O3 <: IntT,
    O4 <: IntT,
    O5 <: IntT,
    O6 <: IntT,
  ](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
    L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
  ] = x * 1e-3

  inline def centi[
    L <: IntT,
    T <: IntT,
    P <: IntT,
    M <: IntT,
    Q <: IntT,
    N <: IntT,
    C <: IntT,
    A <: IntT,
    AQ <: IntT,
    AP <: IntT,
    O1 <: IntT,
    O2 <: IntT,
    O3 <: IntT,
    O4 <: IntT,
    O5 <: IntT,
    O6 <: IntT,
  ](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
    L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
  ] = x * 1e-2

  inline def deci[
    L <: IntT,
    T <: IntT,
    P <: IntT,
    M <: IntT,
    Q <: IntT,
    N <: IntT,
    C <: IntT,
    A <: IntT,
    AQ <: IntT,
    AP <: IntT,
    O1 <: IntT,
    O2 <: IntT,
    O3 <: IntT,
    O4 <: IntT,
    O5 <: IntT,
    O6 <: IntT,
  ](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
    L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
  ] = x * 1e-1

  inline def kilo[
    L <: IntT,
    T <: IntT,
    P <: IntT,
    M <: IntT,
    Q <: IntT,
    N <: IntT,
    C <: IntT,
    A <: IntT,
    AQ <: IntT,
    AP <: IntT,
    O1 <: IntT,
    O2 <: IntT,
    O3 <: IntT,
    O4 <: IntT,
    O5 <: IntT,
    O6 <: IntT,
  ](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
    L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
  ] = x * 1e3

  inline def mega[
    L <: IntT,
    T <: IntT,
    P <: IntT,
    M <: IntT,
    Q <: IntT,
    N <: IntT,
    C <: IntT,
    A <: IntT,
    AQ <: IntT,
    AP <: IntT,
    O1 <: IntT,
    O2 <: IntT,
    O3 <: IntT,
    O4 <: IntT,
    O5 <: IntT,
    O6 <: IntT,
  ](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
    L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
  ] = x * 1e6

  inline def giga[
    L <: IntT,
    T <: IntT,
    P <: IntT,
    M <: IntT,
    Q <: IntT,
    N <: IntT,
    C <: IntT,
    A <: IntT,
    AQ <: IntT,
    AP <: IntT,
    O1 <: IntT,
    O2 <: IntT,
    O3 <: IntT,
    O4 <: IntT,
    O5 <: IntT,
    O6 <: IntT,
  ](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
    L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
  ] = x * 1e9

  inline def tera[
    L <: IntT,
    T <: IntT,
    P <: IntT,
    M <: IntT,
    Q <: IntT,
    N <: IntT,
    C <: IntT,
    A <: IntT,
    AQ <: IntT,
    AP <: IntT,
    O1 <: IntT,
    O2 <: IntT,
    O3 <: IntT,
    O4 <: IntT,
    O5 <: IntT,
    O6 <: IntT,
  ](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
    L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
  ] = x * 1e12

  inline def peta[
    L <: IntT,
    T <: IntT,
    P <: IntT,
    M <: IntT,
    Q <: IntT,
    N <: IntT,
    C <: IntT,
    A <: IntT,
    AQ <: IntT,
    AP <: IntT,
    O1 <: IntT,
    O2 <: IntT,
    O3 <: IntT,
    O4 <: IntT,
    O5 <: IntT,
    O6 <: IntT,
  ](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
    L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
  ] = x * 1e15

  inline def kibi[
    L <: IntT,
    T <: IntT,
    P <: IntT,
    M <: IntT,
    Q <: IntT,
    N <: IntT,
    C <: IntT,
    A <: IntT,
    AQ <: IntT,
    AP <: IntT,
    O1 <: IntT,
    O2 <: IntT,
    O3 <: IntT,
    O4 <: IntT,
    O5 <: IntT,
    O6 <: IntT,
  ](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
    L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
  ] = x * 1024

  inline def mebi[
    L <: IntT,
    T <: IntT,
    P <: IntT,
    M <: IntT,
    Q <: IntT,
    N <: IntT,
    C <: IntT,
    A <: IntT,
    AQ <: IntT,
    AP <: IntT,
    O1 <: IntT,
    O2 <: IntT,
    O3 <: IntT,
    O4 <: IntT,
    O5 <: IntT,
    O6 <: IntT,
  ](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
    L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
  ] = x * math.pow(1024, 2)

  inline def gibi[
    L <: IntT,
    T <: IntT,
    P <: IntT,
    M <: IntT,
    Q <: IntT,
    N <: IntT,
    C <: IntT,
    A <: IntT,
    AQ <: IntT,
    AP <: IntT,
    O1 <: IntT,
    O2 <: IntT,
    O3 <: IntT,
    O4 <: IntT,
    O5 <: IntT,
    O6 <: IntT,
  ](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
    L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
  ] = x * math.pow(1024, 3)

  inline def tebi[
    L <: IntT,
    T <: IntT,
    P <: IntT,
    M <: IntT,
    Q <: IntT,
    N <: IntT,
    C <: IntT,
    A <: IntT,
    AQ <: IntT,
    AP <: IntT,
    O1 <: IntT,
    O2 <: IntT,
    O3 <: IntT,
    O4 <: IntT,
    O5 <: IntT,
    O6 <: IntT,
  ](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
    L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
  ] = x * math.pow(1024, 4)

  inline def pebi[
    L <: IntT,
    T <: IntT,
    P <: IntT,
    M <: IntT,
    Q <: IntT,
    N <: IntT,
    C <: IntT,
    A <: IntT,
    AQ <: IntT,
    AP <: IntT,
    O1 <: IntT,
    O2 <: IntT,
    O3 <: IntT,
    O4 <: IntT,
    O5 <: IntT,
    O6 <: IntT,
  ](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
    L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
  ] = x * math.pow(1024, 5)
end AdditionalUnits
