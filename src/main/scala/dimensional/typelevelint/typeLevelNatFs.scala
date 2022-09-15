package dimensional.typelevelint

/**
 * First projection of a pair type
 */
type First[Pair] = Pair match
  case (first, _) => first

/**
 * Second projection of a pair type
 */
type Second[Pair] = Pair match
  case (_, second) => second

/**
 * Type-level sum of two nats
 */
type NatSum[M <: NatT, N <: NatT] <: NatT = (M, N) match
  case (_, `_0`) => M
  case (`_0`, _) => N
  case (Succ[predM], Succ[predN]) => Succ[Succ[NatSum[predM, predN]]]

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
 * Helper type for type-level NatT division with remainder
 *
 * @tparam M the dividend
 * @tparam N the divisor
 * @tparam QAcc the accumulator for the quotient
 */
type NatQuotientHelper[M <: NatT, N <: PositiveT, QAcc <: NatT] = NatDiff[M, N] match
  case Minus[_] => (QAcc, M)
  case `_0` => (Succ[QAcc], _0)
  case Succ[diffPred] => NatQuotientHelper[Succ[diffPred], N, Succ[QAcc]]

/**
 * Helper type for type-level NatT division without remainder.
 *
 * This is defined separately because of this bug: https://github.com/lampepfl/dotty/issues/15816
 *
 * @tparam M the dividend
 * @tparam N the divisor
 * @tparam QAcc the accumulator for the quotient
 */
type NatQuotientFloorHelper[M <: NatT, N <: PositiveT, QAcc <: NatT] <: NatT = NatDiff[M, N] match
  case Minus[_] => QAcc
  case `_0` => Succ[QAcc]
  case Succ[diffPred] => NatQuotientFloorHelper[Succ[diffPred], N, Succ[QAcc]]

/**
 * Type-level NatT division with remainder
 *
 * @tparam M the dividend
 * @tparam N the divisor
 */
type NatQuotient[M <: NatT, N <: PositiveT] = Succ[N] match
  case `_1` => (M, _0)
  case _ => NatQuotientHelper[M, N, _0]

/**
 * Type-level rounded-down NatT division
 *
 * @tparam M the dividend
 * @tparam N the divisor
 */
type NatQuotientFloor[M <: NatT, N <: PositiveT] = First[NatQuotient[M, N]]

/**
 * Alternative type-level rounded-down NatT division
 *
 * This is defined because of this bug: https://github.com/lampepfl/dotty/issues/15816
 *
 * @tparam M the dividend
 * @tparam N the divisor
 */
type NatQuotientFloor2[M <: NatT, N <: PositiveT] <: NatT = M match
  case _ => NatQuotientFloorHelper[M, N, _0]

/**
 * Type-level NatT modulo operation
 */
type NatRemainder[M <: NatT, N <: PositiveT] = Second[NatQuotient[M, N]]

/**
 * Type-level product of two nats
 */
type NatProd[M <: NatT, N <: NatT] <: NatT = (M, N) match
  case (_, `_0`) => _0
  case (_, `_1`) => M
  case (`_0`, _) => _0
  case (`_1`, _) => N
  case (Succ[predM], Succ[predN]) => NatSum[NatSum[_1, NatSum[predM, predN]], NatProd[predM, predN]]
