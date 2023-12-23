package dimensional.dimension

import dimensional.typelevelint.{_0, _1, _2, _3}
import dimensional.dimension.Dimensions.Dim

/**
 * Trivial dimension to represent dimensionless quantities
 */
type Uno = Dim[_0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0]

// Base dimensions
type Length            = Dim[_1, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0]
type Time              = Dim[_0, _1, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0]
type Temperature       = Dim[_0, _0, _1, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0]
type Mass              = Dim[_0, _0, _0, _1, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0]
type ElectricCharge    = Dim[_0, _0, _0, _0, _1, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0]
type SubstanceAmount   = Dim[_0, _0, _0, _0, _0, _1, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0]
type Cost              = Dim[_0, _0, _0, _0, _0, _0, _1, _0, _0, _0, _0, _0, _0, _0, _0, _0]
type Angle             = Dim[_0, _0, _0, _0, _0, _0, _0, _1, _0, _0, _0, _0, _0, _0, _0, _0]
type AbstractCharge    = Dim[_0, _0, _0, _0, _0, _0, _0, _0, _1, _0, _0, _0, _0, _0, _0, _0]
type AbstractPotential = Dim[_0, _0, _0, _0, _0, _0, _0, _0, _0, _1, _0, _0, _0, _0, _0, _0]
type Other1            = Dim[_0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _1, _0, _0, _0, _0, _0]
type Other2            = Dim[_0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _1, _0, _0, _0, _0]
type Other3            = Dim[_0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _1, _0, _0, _0]
type Other4            = Dim[_0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _1, _0, _0]
type SolidAngle        = Dim[_0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _1, _0]
type Information       = Dim[_0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _0, _1]

// Derived dimensions
type AbstractChargeDensity = AbstractCharge / Volume
type AbstractConductivity = AbstractCharge / AbstractPotential / Length / Time
type AbstractCurrent = AbstractCharge / Time
type AbstractResistivity = Uno / AbstractConductivity
type AbstractVolumetricCapacity = AbstractCharge / AbstractPotential / Volume
type Acceleration = Velocity / Time
type Area = Length ~ _2
type Density = Mass / Volume
type Diffusivity = Area / Time
type ElectricCurrent = ElectricCharge / Time
type ElectricPotential = Energy / ElectricCharge
type ElectricResistance = ElectricPotential / ElectricCurrent
type Energy = Force * Length
type Force = Mass * Acceleration
type Frequency = Uno / Time
type Momentum = Mass * Velocity
type Power = Energy / Time
type Pressure = Force / Area
type Velocity = Length / Time
type Viscosity = Pressure * Time
type Volume = Length ~ _3
