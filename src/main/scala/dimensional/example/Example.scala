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

  summon[Density =:= WithChargeSetTo[AbstractChargeDensity, Mass]]
  summon[Density =:= WithPotentialSetTo[WithChargeSetTo[AbstractVolumetricCapacity, Momentum], Velocity]]
  summon[ElectricCurrent =:= WithChargeSetTo[AbstractCurrent, ElectricCharge]]

//  val incorrect1 = v1 + lightYear
//  val incorrect2: Time = 1 * metre
//  println(litre.root(_2).asString)
