package dimensional

object IntType:

  /**
   * Type-level Int type
   */
  sealed trait IntT

  /**
   * Non-zero IntT type
   */
  sealed trait NonZeroIntT extends IntT

  /**
   * Type-level Nat type
   */
  sealed trait NatT extends IntT

  /**
   * Type-level 0 type (and term/value)
   */
  final case class Zero() extends NatT

  /**
   * Type-level non-zero Nat type
   * @param n the predecessor term/value
   * @tparam N the predecessor type
   */
  final case class Succ[+N <: NatT](n: N) extends NatT with NonZeroIntT

  /**
   * Type-level negative Int type
   * @param n the absolute value term/value
   * @tparam N the absolute value type
   */
  final case class Minus[+N <: Succ[NatT]](n: N) extends NonZeroIntT

  /**
   * Type-level Boolean type
   */
  sealed trait BoolT

  /**
   * Type-level "true" type
   */
  final case class Bot() extends BoolT

  /**
   * Type-level "false" type
   */
  final case class Top() extends BoolT

  /**
   * Type-level sum of two nats
   */
  type NatSum[M <: NatT, N <: NatT] <: NatT = (M, N) match
    case (_, `_0`) => M
    case (`_0`, _) => N
    case (Succ[predM], Succ[predN]) => Succ[Succ[NatSum[predM, predN]]]

  // TODO can the signature somehow be refined to "natSum[X <: NatT, Y <: NatT](x: X, y: Y): NatSum[X, Y]"?
  /**
   * Sum of two NatTs
   */
  def natSum(m: NatT, n: NatT): NatT = (m, n) match
    case (_, Zero()) => m
    case (Zero(), _) => n
    case (Succ(predM), Succ(predN)) => Succ(Succ(natSum(predM, predN)))

  /**
   * First projection of a pair type
   */
  type First[Pair] = Pair match
    case (x, _) => x

  /**
   * Second projection of a pair type
   */
  type Second[Pair] = Pair match
    case (_, y) => y

  /**
   * Type-level difference between two nats
   */
  type NatDiff[M <: NatT, N <: NatT] <: IntT = (M, N) match
    case (_, `_0`) => M
    case (`_0`, _) => Minus[N]
    case (Succ[predM], Succ[predN]) => NatDiff[predM, predN]

  /**
   * Type level less-than comparison between two nats
   */
  type NatLessThan[M <: NatT, N <: NatT] = NatDiff[M, N] match
    case Minus[_] => Top
    case _ => Bot

  /**
   * Type representing that one nat is less than another
   *
   * @tparam M the minuend
   * @tparam N the subtrahend
   */
  trait NatIsLessThan[M <: NatT, N <: NatT]
  object NatIsLessThan:
    given[M <: NatT, N <: NatT] (using NatLessThan[M, N] =:= Top): NatIsLessThan[M, N]()

  /**
   * Helper type for type-level Nat division with remainder
   *
   * @tparam M the dividend
   * @tparam N the divisor
   * @tparam QAcc the accumulator for the quotient
   */
  type NatQuotientHelper[M <: NatT, N <: Succ[NatT], QAcc <: NatT] = NatDiff[M, N] match
    case Minus[_] => (QAcc, M)
    case `_0` => (Succ[QAcc], _0)
    case Succ[diffPred] => NatQuotientHelper[Succ[diffPred], N, Succ[QAcc]]

  /**
   * Type-level Nat division with remainder
   *
   * @tparam M the dividend
   * @tparam N the divisor
   */
  type NatQuotient[M <: NatT, N <: Succ[NatT]] = N match
    case `_1` => (M, _0)
    case _ => NatQuotientHelper[M, N, _0]

  /**
   * Type-level rounded-down Nat division
   *
   * @tparam M the dividend
   * @tparam N the divisor
   */
  type NatQuotientFloor[M <: NatT, N <: Succ[NatT]] = First[NatQuotient[M, N]]

  /**
   * Type-level Nat modulo operation
   */
  type NatRemainder[M <: NatT, N <: Succ[NatT]] = Second[NatQuotient[M, N]]

  /**
   * Type-level Int division, rounded towards 0
   * @tparam I the dividend
   * @tparam J the divisor
   */
  type IntQuotient[I <: IntT, J <: NonZeroIntT] = (I, J) match
    case (_, `_1`) => (I, _0)
    case (Minus[absI], Minus[absJ]) => NatQuotientFloor[absI, absJ]
    case (_, Minus[absJ]) => Minus[NatQuotientFloor[I, absJ]]
    case (Minus[absI], _) => Minus[NatQuotientFloor[absI, J]]
    case _ => NatQuotientFloor[I, J]

  /**
   * Type-level Int negation
   */
  type Neg[I <: IntT] <: IntT = I match
    case _0 => _0
    case Minus[absI] => absI
    case _ => Minus[I]

  /**
   * Type-level sum of two ints
   */
  type Sum[X <: IntT, Y <: IntT] <: IntT = (X, Y) match
    case (_, `_0`) => X
    case (Minus[x], Minus[y]) => Minus[NatSum[x, y]]
    case (_, Minus[y]) => NatDiff[X, y]
    case (Minus[x], _) => NatDiff[Y, x]
    case _ => NatSum[X, Y]

  /**
   * Type-level difference between two ints
   *
   * @tparam I the minuend
   * @tparam J the subtrahend
   */
  type Diff[I <: IntT, J <: IntT] = Sum[I, Neg[J]]

  /**
   * Type-level product of two nats
   */
  type NatProd[M <: NatT, N <: NatT] <: NatT = (M, N) match
    case (_, `_0`) => _0
    case (_, `_1`) => M
    case (`_0`, _) => _0
    case (`_1`, _) => N
    case (Succ[predM], Succ[predN]) => NatSum[NatSum[_1, NatSum[predM, predN]], NatProd[predM, predN]]

  /**
   * Type-level product of two ints
   */
  type Prod[I <: IntT, J <: IntT] <: IntT = (I, J) match
    case (_, `_0`) => _0
    case (_, `_1`) => I
    case (Minus[absI], Minus[absJ]) => NatProd[absI, absJ]
    case (_, Minus[absJ]) => Minus[NatProd[I, absJ]]
    case (Minus[absI], _) => Minus[NatProd[absI, J]]
    case _ => NatProd[I, J]

  /**
   * Type representing that one nat divides another
   *
   * @tparam N the divisor
   * @tparam M the dividend
   */
  trait NatDivides[N <: Succ[NatT], M <: NatT]
  object NatDivides:
    given [N <: Succ[NatT]]: NatDivides[N, _0]()
    given [N <: Succ[NatT], M <: Succ[NatT]] (using NatRemainder[M, N] =:= _0): NatDivides[N, M]()

  /**
   * Type representing that one int divides another
   *
   * @tparam J the divisor
   * @tparam I the dividend
   */
  trait Divides[J <: IntT, I <: IntT]
  object Divides:
    given [J <: Succ[NatT], I <: NatT] (using NatDivides[J, I]): Divides[J, I]()
    given [J <: Succ[NatT], I <: NatT] (using NatDivides[J, I]): Divides[Minus[J], I]()
    given [J <: Succ[NatT], I <: Succ[NatT]] (using NatDivides[J, I]): Divides[J, Minus[I]]()
    given [J <: Succ[NatT], I <: Succ[NatT]] (using NatDivides[J, I]): Divides[Minus[J], Minus[I]]()

  /**
   * Converts a NatT to an Int.
   */
  def natAsInt(x: NatT): Int = x match
    case _0() => 0
    case Succ(n) => 1 + natAsInt(n)

  /**
   * Converts a non-negative Int to a NatT.
   */
  def intAsNat(x: Int): NatT =
    assert(x >= 0)
    if x == 0 then _0 else Succ(intAsNat(x - 1))

  /**
   * Converts a strictly positive Int to a non-zero NatT.
   */
  def positiveIntAsNat(x: Int): Succ[NatT] =
    assert(x > 0)
    Succ(intAsNat(x - 1))

  /**
   * Converts an IntT to an Int.
   */
  def intTAsInt(x: IntT): Int = x match
    case Minus(n) => -natAsInt(n)
    case n: NatT => natAsInt(n)

  /**
   * Converts an Int to an IntT.
   */
  def intAsIntT(x: Int): IntT = if x < 0 then Minus(positiveIntAsNat(-x)) else intAsNat(x)

  /**
   * Difference between two IntTs
   *
   * @param x the minuend
   * @param y the subtrahend
   */
  def diff(x: IntT, y: IntT): IntT = intAsIntT(intTAsInt(x) - intTAsInt(y))

  /**
   * @param x the base
   * @param i the exponent
   * @return the given Double raised to the given IntT
   */
  def power(x: Double, i: IntT): Double = math.pow(x, intTAsInt(i))

  /**
   * @param x the root argument
   * @param i the type of root to take
   * @return the ith root of the given Double
   */
  def root(x: Double, i: NonZeroIntT): Double = math.pow(x, 1.0 / intTAsInt(i))

  // Type-level convenience aliases
  type _0 = Zero
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]

  // Term/value-level convenience aliases
  val _0: _0 = Zero()
  val _1: _1 = Succ(_0)
  val _2: _2 = Succ(_1)
  val _3: _3 = Succ(_2)
  val _4: _4 = Succ(_3)
  val _5: _5 = Succ(_4)
  val _6: _6 = Succ(_5)
  val _7: _7 = Succ(_6)
  val _8: _8 = Succ(_7)
  val _9: _9 = Succ(_8)

  // givens that allow summoning of an IntT value/term
  given Zero = _0
  given [N <: NatT](using n: N): Succ[N] = Succ(n)
  given [N <: Succ[NatT]](using n: N): Minus[N] = Minus(n)
end IntType
