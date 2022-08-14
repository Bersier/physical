package dimensional.example

import dimensional.dimension.*
import dimensional.dimension.Dimensions.{*, given}
import dimensional.typelevelint.{*, given}

import scala.language.implicitConversions
import scala.util.NotGiven

@main def main(): Unit =
  val v1: Velocity = 0.1 * metre / milli(second)
  val v2: Length / Time = 15 * metre / second
  println((v1 + v2).in(kilo(metre) / hour))
  println((v1 + v2).asStringWith(kilo(metre) / hour, "km/h"))

  val waterDensity = 0.997 * kilogram / litre
  println(waterDensity.asString)

  println(lightYear.in(giga(kilo(metre))))

  println(s"A centipoise is ${centipoise.asString}.")
  println(s"A poise is ${(100 * centipoise).asStringWith(pascal, "Pa")}.")

  val myUnit = kilo(inch) * newton / hour
  println("A poise is " + (100 * centipoise).asStringWith(myUnit, "MU"))

  println(litre.root(_3).asStringWith(centi(meter), "cm"))

  println("A Coulomb is " + coulomb.asStringWith(ampere, "A"))

  summon[_9 =:= NatSum[_2, _7]]
  summon[_1 =:= NatRemainder[_9, _4]]

  summon[NatDivides[_2, _2]]
  summon[NatDivides[_3, _9]]
  summon[NotGiven[NatDivides[_4, _9]]]

//  val incorrect1 = v1 + lightYear
//  val incorrect2: Time = 1 * metre
//  println(litre.root(_2).asString)

/**
 * Makes an abstract dim concrete, using the given concrete charge and potential.
 */
type ConcreteDim[AbstractDim, Charge, Potential] = WithChargeSetTo[WithPotentialSetTo[AbstractDim, Potential], Charge]

/**
 * Shows how abstract units can be made concrete for [heat as charge] and [temperature as potential].
 */
def heat(): Unit =
  type HeatConcretion[AbstractDim] = ConcreteDim[AbstractDim, Heat, Temperature]
  type Heat = Energy
  type ThermalConductivity = Power / Length / Temperature
  type VolumetricHeatCapacity = Energy / Temperature / Volume
  summon[HeatConcretion[AbstractCharge] =:= Heat]
  summon[HeatConcretion[AbstractPotential] =:= Temperature]
  summon[HeatConcretion[AbstractConductivity] =:= ThermalConductivity]
  summon[HeatConcretion[AbstractVolumetricCapacity] =:= VolumetricHeatCapacity]

/**
 * Shows how abstract units can be made concrete for [electric charge as charge] and [electric potential as potential].
 */
def electricity(): Unit =
  type ElectricConcretion[AbstractDim] = ConcreteDim[AbstractDim, ElectricCharge, ElectricPotential]
  type ElectricResistivity = ElectricResistance * Length
  type VolumetricCapacitance = ElectricCharge / ElectricPotential / Volume
  summon[ElectricConcretion[AbstractCharge] =:= ElectricCharge]
  summon[ElectricConcretion[AbstractPotential] =:= ElectricPotential]
  summon[ElectricConcretion[AbstractResistivity] =:= ElectricResistivity]
  summon[ElectricConcretion[AbstractVolumetricCapacity] =:= VolumetricCapacitance]

/**
 * Shows how abstract units can be made concrete for [substance amount as charge] and
 * [substance concentration as potential].
 */
def substance(): Unit =
  type SubstanceConcretion[AbstractDim] = ConcreteDim[AbstractDim, SubstanceAmount, SubstanceConcentration]
  type SubstanceConcentration = SubstanceAmount / Volume
  type MassDiffusivity = Diffusivity
  summon[SubstanceConcretion[AbstractCharge] =:= SubstanceAmount]
  summon[SubstanceConcretion[AbstractPotential] =:= SubstanceConcentration]
  summon[SubstanceConcretion[AbstractConductivity] =:= MassDiffusivity]
  summon[SubstanceConcretion[AbstractVolumetricCapacity] =:= Uno]

/**
 * Shows how abstract units can be made concrete for [momentum as charge] and [velocity as potential].
 */
def momentum(): Unit =
  type SAConcretion[AbstractDim] = ConcreteDim[AbstractDim, Momentum, Velocity]
  type DynamicViscosity = Viscosity
  type KinematicViscosity = Diffusivity
  type MassFlux = Mass / Time / Area
  summon[SAConcretion[AbstractCharge] =:= Momentum]
  summon[SAConcretion[AbstractPotential] =:= Velocity]
  summon[SAConcretion[AbstractConductivity] =:= DynamicViscosity]
  summon[SAConcretion[AbstractVolumetricCapacity] =:= Density]
  summon[SAConcretion[AbstractChargeDensity] =:= MassFlux]
  summon[SAConcretion[Diffusivity] =:= KinematicViscosity]
