package types

import types.IntType.*

import scala.annotation.targetName

object Dimensions:
  //noinspection ScalaUnusedSymbol
  opaque type Dim[
    Length <: IntT,
    Time <: IntT,
    Potential <: IntT,
    Mass <: IntT,
    Quantity <: IntT,
    Cost <: IntT,
  ] = Double

  type DimMap[Op[_ <: IntT] <: IntT, X] = X match
    case Dim[l, t, p, m, q, c] => Dim[Op[l], Op[t], Op[p], Op[m], Op[q], Op[c]]

  type DimMap2[Op[_ <: IntT, _ <: IntT] <: IntT, X, Y] = X match
    case Dim[lx, tx, px, mx, qx, cx] => Y match
      case Dim[ly, ty, py, my, qy, cy] =>
        Dim[Op[lx, ly], Op[tx, ty], Op[px, py], Op[mx, my], Op[qx, qy], Op[cx, cy]]

  @targetName("times") type *[X, Y] = DimMap2[Sum, X, Y]
  @targetName("over") type /[X, Y] = DimMap2[Dif, X, Y]
  @targetName("toThe") type ~[X, Y <: IntT] = DimMap[[Z <: IntT] =>> Prod[Z, Y], X]

  type Uno = Dim[_0, _0, _0, _0, _0, _0]

  type Length          = Dim[_1, _0, _0, _0, _0, _0]
  type Time            = Dim[_0, _1, _0, _0, _0, _0]
  type Temperature     = Dim[_0, _0, _1, _0, _0, _0]
  type Mass            = Dim[_0, _0, _0, _1, _0, _0]
  type ElectricCharge  = Dim[_0, _0, _0, _0, _1, _0]
  type SubstanceAmount = Dim[_0, _0, _0, _0, _0, _1]

  type Acceleration = Length / Time ~ _2
  type Area = Length ~ _2
  type Density = Mass / Volume
  type ElectricCurrent = ElectricCharge / Time
  type ElectricPotential = Energy / ElectricCharge
  type Energy = Mass * Area / Time ~ _2
  type Force = Energy / Length
  type Frequency = Time ~ Minus[_1]
  type Power = Energy / Time
  type Pressure = Force / Area
  type Velocity = Length / Time
  type Viscosity = Pressure * Time
  type Volume = Length ~ _3

  val ampere  : ElectricCurrent   = 1
  val coulomb : ElectricCharge    = 1
  val hertz   : Frequency         = 1
  val joule   : Energy            = 1
  val kelvin  : Temperature       = 1
  val kilogram: Mass              = 1
  val metre   : Length            = 1
  val mole    : SubstanceAmount   = 1
  val newton  : Force             = 1
  val pascal  : Pressure          = 1
  val second  : Time              = 1
  val volt    : ElectricPotential = 1
  val watt    : Power             = 1

  given Conversion[Double, Uno] with
    inline def apply(d: Double): Uno = d

  given Conversion[Int, Uno] with
    inline def apply(d: Int): Uno = d.toDouble

  given Conversion[Uno, Double] with
    inline def apply(d: Uno): Double = d

  extension[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, C <: IntT
  ] (x: Dim[L, T, P, M, Q, C])
    inline def in(units: Dim[L, T, P, M, Q, C]): Double = x / units
    @targetName("plus") inline def +(y: Dim[L, T, P, M, Q, C]): Dim[L, T, P, M, Q, C] = x + y
    @targetName("minus") inline def -(y: Dim[L, T, P, M, Q, C]): Dim[L, T, P, M, Q, C] = x - y
    @targetName("times") inline def *[
      Ly <: IntT, Ty <: IntT, Py <: IntT, My <: IntT, Qy <: IntT, Cy <: IntT
    ](y: Dim[Ly, Ty, Py, My, Qy, Cy]): Dim[L, T, P, M, Q, C] * Dim[Ly, Ty, Py, My, Qy, Cy] = x * y
    @targetName("over") inline def /[
      Ly <: IntT, Ty <: IntT, Py <: IntT, My <: IntT, Qy <: IntT, Cy <: IntT
    ](y: Dim[Ly, Ty, Py, My, Qy, Cy]): Dim[L, T, P, M, Q, C] / Dim[Ly, Ty, Py, My, Qy, Cy] = x / y
    @targetName("toThe") inline def ~[E <: IntT](y: E): Dim[L, T, P, M, Q, C] ~ E = power(x, y)
end Dimensions
