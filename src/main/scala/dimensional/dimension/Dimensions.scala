package dimensional.dimension

import dimensional.typelevelint
import dimensional.typelevelint.{Divides, IntT, NonZeroIntT, diff, power}

import scala.annotation.targetName
import scala.language.implicitConversions

/**
 * Where the main definitions are, in particular Dim for physical quantities.
 */
object Dimensions:
  /**
   * Type that keeps track of the dimension of a quantity.
   *
   * @tparam Length exponent for the length component of the dim
   * @tparam Time exponent for the time component of the dim
   * @tparam Temperature exponent for the temperature component of the dim
   * @tparam Mass exponent for the mass component of the dim
   * @tparam ElectricCharge exponent for the electric charge component of the dim
   * @tparam SubstanceAmount exponent for the substance amount component of the dim
   * @tparam Cost exponent for the cost component of the dim
   * @tparam Angle exponent for the angle component of the dim
   * @tparam AbstractCharge exponent for the abstract charge component of the dim
   * @tparam AbstractPotential exponent for the abstract potential component of the dim
   * @tparam Other1 exponent for the other1 component of the dim
   * @tparam Other2 exponent for the other2 component of the dim
   * @tparam Other3 exponent for the other3 component of the dim
   * @tparam Other4 exponent for the other4 component of the dim
   * @tparam SolidAngle exponent for the solid angle component of the dim
   * @tparam Information exponent for the information component of the dim
   */
  opaque type Dim[
    Length <: IntT,            // l
    Time <: IntT,              // t
    Temperature <: IntT,       // p
    Mass <: IntT,              // m
    ElectricCharge <: IntT,    // q
    SubstanceAmount <: IntT,   // n
    Cost <: IntT,              // c
    Angle <: IntT,             // a
    AbstractCharge <: IntT,    // aQ
    AbstractPotential <: IntT, // aP
    Other1 <: IntT,            // o1
    Other2 <: IntT,            // o2
    Other3 <: IntT,            // o3
    Other4 <: IntT,            // o4
    SolidAngle <: IntT,        // s
    Information <: IntT,       // b
  ] = Double

  // Standard units (SI units for SI dimensions)
  val ampere   : ElectricCurrent    = 1
  val coulomb  : ElectricCharge     = 1
  val dollar   : Cost               = 1
  val hertz    : Frequency          = 1
  val joule    : Energy             = 1
  val kelvin   : Temperature        = 1
  val kilogram : Mass               = 1
  val metre    : Length             = 1
  val mole     : SubstanceAmount    = 1
  val nat      : Information        = 1
  val newton   : Force              = 1
  val o1Unit   : Other1             = 1
  val o2Unit   : Other2             = 1
  val o3Unit   : Other3             = 1
  val o4Unit   : Other4             = 1
  val ohm      : ElectricResistance = 1
  val pascal   : Pressure           = 1
  val pUnit    : AbstractPotential  = 1
  val rUnit    : AbstractCharge     = 1
  val radian   : Angle              = 1
  val second   : Time               = 1
  val steradian: SolidAngle         = 1
  val volt     : ElectricPotential  = 1
  val watt     : Power              = 1

  /**
   * Dimensionless quantities can be auto-converted to Doubles.
   */
  given Conversion[Double, Uno] with
    inline def apply(d: Double): Uno = d

  /**
   * Int can be auto-converted to Uno.
   */
  given Conversion[Int, Uno] with
    inline def apply(d: Int): Uno = d.toDouble

  /**
   * Double can be auto-converted to Uno.
   */
  given Conversion[Uno, Double] with
    inline def apply(d: Uno): Double = d

  // Trigonometric functions
  inline def sin(a: Angle): Uno = math.sin(a)
  inline def cos(a: Angle): Uno = math.cos(a)
  inline def tan(a: Angle): Uno = math.tan(a)
  inline def sec(a: Angle): Uno = 1 / math.cos(a)
  inline def csc(a: Angle): Uno = 1 / math.sin(a)
  inline def cot(a: Angle): Uno = 1 / math.tan(a)

  extension (a: Angle)
    /**
     * Normalizes this angle two be between 0 and 2 Pi.
     */
    inline def normalized: Angle = (tau * fractionalPart((a: Double) / tau))

  // Inverse trigonometric functions
  inline def asin(x: Uno): Angle = math.asin(x)
  inline def acos(x: Uno): Angle = math.acos(x)
  inline def atan(x: Uno): Angle = math.atan(x)
  inline def asec(x: Uno): Angle = math.acos(1 / x)
  inline def acsc(x: Uno): Angle = math.asin(1 / x)
  inline def acot(x: Uno): Angle = math.atan(1 / x)

  /**
   * @param x the (effective) number of values in the discrete uniform distribution
   * @return the amount of entropy in a discrete uniform distribution with the given number of values
   */
  inline def log(x: Uno): Information = math.log(x)

  /**
   * @param x the entropy of the discrete uniform distribution
   * @return the (effective) number of values in a discrete uniform distribution with the given entropy
   */
  inline def exp(x: Information): Uno = math.exp(x)

  /**
   * Absolute value
   */
  inline def abs[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
    O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, S <: IntT, B <: IntT,
  ](
    x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B]
  ): Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B] = math.abs(x)

  /**
   * Floor
   */
  inline def floor[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
    O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, S <: IntT, B <: IntT,
  ](
    x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B]
  ): Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B] = math.floor(x)

  /**
   * Ceiling
   */
  inline def ceil[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
    O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, S <: IntT, B <: IntT,
  ](
    x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B]
  ): Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B] = math.ceil(x)

  /**
   * Functions that apply to any quantity, regardless of its dimension.
   */
  extension[
    L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
    O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, S <: IntT, B <: IntT,
  ] (x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B])

    /**
     * String representation of this quantity, using base dimensions and standard units
     */
    def asString(using L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B): String =
      x.toString + " " + dimensionsAsString(
        summon[L], summon[T], summon[P], summon[M], summon[Q], summon[N], summon[C], summon[A], summon[AQ], summon[AP],
        summon[O1], summon[O2], summon[O3], summon[O4], summon[S], summon[B]
      )

    /**
     * String representation of this quantity, using the given unit, as well as base dimensions and standard units
     */
    def asStringWith[
      L2 <: IntT, T2 <: IntT, P2 <: IntT, M2 <: IntT, Q2 <: IntT, N2 <: IntT, C2 <: IntT, A2 <: IntT, AQ2 <: IntT,
      AP2 <: IntT, O12 <: IntT, O22 <: IntT, O32 <: IntT, O42 <: IntT, S2 <: IntT, B2 <: IntT,
    ](unit: Dim[L2, T2, P2, M2, Q2, N2, C2, A2, AQ2, AP2, O12, O22, O32, O42, S2, B2], unitString: String)(using
      l: L, t: T, p: P, m: M, q: Q, n: N, c: C, a: A, aQ: AQ, aP: AP, o1: O1, o2: O2, o3: O3, o4: O4, s: S, b: B,
      l2: L2, t2: T2, p2: P2, m2: M2, q2: Q2, n2: N2, c2: C2, a2: A2, aQ2: AQ2, aP2: AP2, o12: O12, o22: O22, o32: O32,
      o42: O42, s2: S2, b2: B2,
    ): String =
      val remainingUnits = dimensionsAsString(
        diff(l, l2), diff(t, t2), diff(p, p2), diff(m, m2), diff(q, q2), diff(n, n2), diff(c, c2), diff(a, a2),
        diff(aQ, aQ2), diff(aP, aP2), diff(o1, o12), diff(o2, o22), diff(o3, o32), diff(o4, o42), diff(s, s2),
        diff(b, b2),
      )
      (x / unit).toString + " " + Seq(unitString, remainingUnits).filter(_.nonEmpty).mkString("·")

    /**
     * @return the magnitude of this quantity in the given unit
     */
    inline def in(unit: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B]): Double = x / unit

    /**
     * Usual smaller-than comparison; only defined if the two quantities to be compared have the same dimension
     */
    @targetName("smallerThan") inline def <(
      y: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B]
    ): Boolean =
      assert(!(x.isNaN || y.isNaN))
      x < y

    /**
     * Usual larger-than comparison; only defined if the two quantities to be compared have the same dimension
     */
    @targetName("largerThan") inline def >(
      y: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B]
    ): Boolean =
      assert(!(x.isNaN || y.isNaN))
      x > y

    /**
     * Usual smaller-or-equal comparison; only defined if the two quantities to be compared have the same dimension
     */
    @targetName("smallerOrEqual") inline def <=(
      y: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B]
    ): Boolean =
      assert(!(x.isNaN || y.isNaN))
      x <= y

    /**
     * Usual larger-or-equal comparison; only defined if the two quantities to be compared have the same dimension
     */
    @targetName("largerOrEqual") inline def >=(
      y: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B]
    ): Boolean =
      assert(!(x.isNaN || y.isNaN))
      x >= y

    /**
     * Usual equality; only defined if the two quantities to be compared have the same dimension
     */
    @targetName("equal") inline def =:=(
      y: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B]
    ): Boolean =
      assert(!(x.isNaN || y.isNaN))
      x == y

    /**
     * Usual addition; only defined if the two quantities to be added have the same dimension
     */
    @targetName("plus") inline def +(
      y: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B]
    ): Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B] = x + y

    /**
     * Usual subtraction; only defined if the two quantities to be subtracted have the same dimension
     */
    @targetName("minus") inline def -(
      y: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B]
    ): Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B] = x - y

    /**
     * Negation
     */
    inline def unary_- : Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B] = -x

    /**
     * Usual multiplication; dimensions are also multiplied
     */
    @targetName("times") inline def *[
      Ly <: IntT, Ty <: IntT, Py <: IntT, My <: IntT, Qy <: IntT, Ny <: IntT, Cy <: IntT, Ay <: IntT, AQy <: IntT,
      APy <: IntT, O1y <: IntT, O2y <: IntT, O3y <: IntT, O4y <: IntT, Sy <: IntT, By <: IntT,
    ](
      y: Dim[Ly, Ty, Py, My, Qy, Ny, Cy, Ay, AQy, APy, O1y, O2y, O3y, O4y, Sy, By]
     ): Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B] *
        Dim[Ly, Ty, Py, My, Qy, Ny, Cy, Ay, AQy, APy, O1y, O2y, O3y, O4y, Sy, By] = x * y

    /**
     * Usual % operator (Behaves like the Scala % operator on Doubles.)
     */
    @targetName("modulo") inline def %[
      Ly <: IntT, Ty <: IntT, Py <: IntT, My <: IntT, Qy <: IntT, Ny <: IntT, Cy <: IntT, Ay <: IntT, AQy <: IntT,
      APy <: IntT, O1y <: IntT, O2y <: IntT, O3y <: IntT, O4y <: IntT, Sy <: IntT, By <: IntT,
    ](
      y: Dim[Ly, Ty, Py, My, Qy, Ny, Cy, Ay, AQy, APy, O1y, O2y, O3y, O4y, Sy, By]
     ): Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B] = x % y

    /**
     * Usual division; dimensions are also divided
     */
    @targetName("over") inline def /[
      Ly <: IntT, Ty <: IntT, Py <: IntT, My <: IntT, Qy <: IntT, Ny <: IntT, Cy <: IntT, Ay <: IntT, AQy <: IntT,
      APy <: IntT, O1y <: IntT, O2y <: IntT, O3y <: IntT, O4y <: IntT, Sy <: IntT, By <: IntT,
    ](
       y: Dim[Ly, Ty, Py, My, Qy, Ny, Cy, Ay, AQy, APy, O1y, O2y, O3y, O4y, Sy, By]
     ): Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B] /
      Dim[Ly, Ty, Py, My, Qy, Ny, Cy, Ay, AQy, APy, O1y, O2y, O3y, O4y, Sy, By] = x / y

    /**
     * Usual exponentiation; dimensions are also exponentiated
     */
    @targetName("toThe") inline def ~[E <: IntT](
      y: E
    ): Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B] ~ E = power(x, y)

    /**
     * @param q the value/quantity to which the abstract charge is to be set
     * @return a quantity equivalent to this one, when the abstract charge is set to the given value
     */
    inline def withAbstractChargeUnitSetTo[
      Lq <: IntT, Tq <: IntT, Pq <: IntT, Mq <: IntT, Qq <: IntT, Nq <: IntT, Cq <: IntT, Aq <: IntT, AQq <: IntT,
      APq <: IntT, O1q <: IntT, O2q <: IntT, O3q <: IntT, O4q <: IntT, Sq <: IntT, Bq <: IntT,
    ](q: Dim[Lq, Tq, Pq, Mq, Qq, Nq, Cq, Aq, AQq, APq, O1q, O2q, O3q, O4q, Sq, Bq])(using qPower: T): WithChargeSetTo[
      Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B],
      Dim[Lq, Tq, Pq, Mq, Qq, Nq, Cq, Aq, AQq, APq, O1q, O2q, O3q, O4q, Sq, Bq],
    ] = x * power(q, qPower)

    /**
     * @param p the value/quantity to which the abstract potential is to be set
     * @return a quantity equivalent to this one, when the abstract potential is set to the given value
     */
    inline def withAbstractPotentialUnitSetTo[
      Lp <: IntT, Tp <: IntT, Pp <: IntT, Mp <: IntT, Qp <: IntT, Np <: IntT, Cp <: IntT, Ap <: IntT, AQp <: IntT,
      APp <: IntT, O1p <: IntT, O2p <: IntT, O3p <: IntT, O4p <: IntT, Sp <: IntT, Bp <: IntT,
    ](
      p: Dim[Lp, Tp, Pp, Mp, Qp, Np, Cp, Ap, AQp, APp, O1p, O2p, O3p, O4p, Sp, Bp]
    )(using pPower: T): WithPotentialSetTo[
      Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B],
      Dim[Lp, Tp, Pp, Mp, Qp, Np, Cp, Ap, AQp, APp, O1p, O2p, O3p, O4p, Sp, Bp],
    ] = x * power(p, pPower)

    /**
     * @return the nth root of this quantity
     */
    inline def root[E <: NonZeroIntT](n: E)(using
      Divides[E, L], Divides[E, T], Divides[E, P], Divides[E, M], Divides[E, Q], Divides[E, N], Divides[E, C],
      Divides[E, A], Divides[E, AQ], Divides[E, AP], Divides[E, O1], Divides[E, O2], Divides[E, O3], Divides[E, O4],
      Divides[E, S], Divides[E, B],
    ): Root[Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, S, B], E] = typelevelint.root(x, n)
end Dimensions
