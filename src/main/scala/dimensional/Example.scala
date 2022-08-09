package dimensional

import dimensional.AdditionalUnits.*
import dimensional.Dimensions.{*, given}
import dimensional.IntType.*

import scala.language.implicitConversions
import scala.util.NotGiven

object Example:
  @main def main(): Unit =
    val v1: Velocity      = 0.1 * metre / milli(second)
    val v2: Length / Time = 15 * metre / second
    println((v1 + v2).in(kilo(metre) / hour))

    val waterDensity = 0.997 * kilogram / litre

    println(lightYear.in(giga(kilo(metre))))

    println(s"A centipoise is ${centipoise.asString}")
    println("A poise is " + (100 * centipoise).asStringWith(pascal, "Pa"))

    println(_9: NatSum[_2, _7])
    println(_1: NatRemainder[_9, _4])
    summon[NatDivides[_2, _2]]
    summon[NatDivides[_3, _9]]
    summon[NotGiven[NatDivides[_4, _9]]]

    summon[Density =:= WithChargeSetTo[AbstractChargeDensity, Mass]]
    summon[Density =:= WithPotentialSetTo[WithChargeSetTo[AbstractVolumetricCapacity, Momentum], Velocity]]
    summon[ElectricCurrent =:= WithChargeSetTo[AbstractCurrent, ElectricCharge]]

//    val incorrect1 = v1 + lightYear
//    val incorrect2: Time = 1 * metre

  extension[
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
    S <: IntT,
    B <: IntT,
  ] (x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B])
    def asStringWith[
      L2 <: IntT,
      T2 <: IntT,
      P2 <: IntT,
      M2 <: IntT,
      Q2 <: IntT,
      N2 <: IntT,
      C2 <: IntT,
      A2 <: IntT,
      AQ2 <: IntT,
      AP2 <: IntT,
      O12 <: IntT,
      O22 <: IntT,
      O32 <: IntT,
      O42 <: IntT,
      S2 <: IntT,
      B2 <: IntT,
    ](quantity: Dim[L2, T2, P2, M2, Q2, N2, C2, A2, AQ2, AP2, O12, O22, O32, O42, S2, B2], unitString: String)(using
      l: L, t: T, p: P, m: M, q: Q, n: N, c: C, a: A, aQ: AQ, aP: AP, o1: O1, o2: O2, o3: O3, o4: O4, s: S, b: B,
      l2: L2, t2: T2, p2: P2, m2: M2, q2: Q2, n2: N2, c2: C2, a2: A2, aQ2: AQ2, aP2: AP2, o12: O12, o22: O22, o32: O32, o42: O42, s2: S2, b2: B2,
    ): String =
      val remainingUnits = dimensionsAsString(
        diff(l, l2),
        diff(t, t2),
        diff(p, p2),
        diff(m, m2),
        diff(q, q2),
        diff(n, n2),
        diff(c, c2),
        diff(a, a2),
        diff(aQ, aQ2),
        diff(aP, aP2),
        diff(o1, o12),
        diff(o2, o22),
        diff(o3, o32),
        diff(o4, o42),
        diff(s, s2),
        diff(b, b2),
      )
      (x / quantity).toString + " " + remainingUnits + "·" + unitString
end Example
