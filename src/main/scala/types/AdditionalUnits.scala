package types

import types.Dimensions.{*, given}
import types.IntType.*

import scala.language.implicitConversions

object AdditionalUnits:
  val atmosphere  : Pressure        = 101325 * pascal
  val bar         : Pressure        = 1e5 * pascal
  val centiPoise  : Pressure * Time = 1e-3 * pascal * second
  val fluidOunce  : Volume          = 29.5735295625 * milli(liter)
  val foot        : Length          = 12 * inch
  val gram        : Mass            = 1e-3 * kilogram
  val hour        : Time            = 3600 * second
  val inch        : Length          = 0.0254 * metre
  val kiloWattHour: Energy          = 3600000 * joule
  val liquidPint  : Volume          = 16 * fluidOunce
  val liquidQuart : Volume          = 2 * liquidPint
  val liter       : Volume          = 1e-3 * metre ~ _3
  val meter       : Length          = 1 * meter
  val mile        : Length          = 1760 * yard
  val minute      : Time            = 60 * second
  val pSI         : Pressure        = 6894.75729317 * pascal
  val poundMass   : Mass            = 0.45359237 * kilogram
  val shortTon    : Mass            = 2000 * poundMass
  val tonne       : Mass            = 1e3 * kilogram
  val usGallon    : Volume          = 4 * liquidQuart
  val wattHour    : Energy          = 3600 * joule
  val yard        : Length          = 3 * foot


  inline def nano[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, C <: IntT
  ](x: Dim[L, T, P, M, Q, C]): Dim[L, T, P, M, Q, C] = x * 1e-9

  inline def micro[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, C <: IntT
  ](x: Dim[L, T, P, M, Q, C]): Dim[L, T, P, M, Q, C] = x * 1e-6

  inline def milli[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, C <: IntT
  ](x: Dim[L, T, P, M, Q, C]): Dim[L, T, P, M, Q, C] = x * 1e-3

  inline def centi[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, C <: IntT
  ](x: Dim[L, T, P, M, Q, C]): Dim[L, T, P, M, Q, C] = x * 1e-2

  inline def deci[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, C <: IntT
  ](x: Dim[L, T, P, M, Q, C]): Dim[L, T, P, M, Q, C] = x * 1e-1

  inline def kilo[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, C <: IntT
  ](x: Dim[L, T, P, M, Q, C]): Dim[L, T, P, M, Q, C] = x * 1e3

  inline def mega[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, C <: IntT
  ](x: Dim[L, T, P, M, Q, C]): Dim[L, T, P, M, Q, C] = x * 1e6

  inline def giga[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, C <: IntT
  ](x: Dim[L, T, P, M, Q, C]): Dim[L, T, P, M, Q, C] = x * 1e9
end AdditionalUnits
