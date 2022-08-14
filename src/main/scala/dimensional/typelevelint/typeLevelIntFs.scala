package dimensional.typelevelint

/**
 * Type-level IntT division, rounded towards 0
 *
 * @tparam I the dividend
 * @tparam J the divisor
 */
type IntQuotient[I <: IntT, J <: NonZeroIntT] <: IntT = (I, J) match
  case (_, `_1`) => I
  case (Minus[absI], Minus[absJ]) => NatQuotientFloor2[absI, absJ]
  case (_, Minus[absJ]) => Neg[NatQuotientFloor2[I, absJ]]
  case (Minus[absI], _) => Neg[NatQuotientFloor2[absI, J]]
  case _ => NatQuotientFloor2[I, J]

/**
 * Type-level IntT negation
 */
type Neg[I <: IntT] <: IntT = I match
  case _0 => _0
  case Minus[absI] => absI
  case _ => Minus[I]

/**
 * Type-level sum of two ints
 */
type Sum[I <: IntT, J <: IntT] <: IntT = (I, J) match
  case (_, `_0`) => I
  case (Minus[absI], Minus[absJ]) => Minus[NatSum[absI, absJ]]
  case (_, Minus[absJ]) => NatDiff[I, absJ]
  case (Minus[absI], _) => NatDiff[J, absI]
  case _ => NatSum[I, J]

/**
 * Type-level difference between two ints
 *
 * @tparam I the minuend
 * @tparam J the subtrahend
 */
type Diff[I <: IntT, J <: IntT] = Sum[I, Neg[J]]

/**
 * Type-level product of two ints
 */
type Prod[I <: IntT, J <: IntT] <: IntT = (I, J) match
  case (_, `_0`) => _0
  case (_, `_1`) => I
  case (`_0`, _) => _0
  case (Minus[absI], Minus[absJ]) => NatProd[absI, absJ]
  case (_, Minus[absJ]) => Minus[NatProd[I, absJ]]
  case (Minus[absI], _) => Minus[NatProd[absI, J]]
  case _ => NatProd[I, J]
