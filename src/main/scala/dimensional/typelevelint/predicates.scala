package dimensional.typelevelint

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
  given [J <: NonZeroIntT, I <: IntT, K <: IntT] (using Divides[J, I]): Divides[J, Prod[K, I]]()
