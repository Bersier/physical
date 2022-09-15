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
trait NatDivides[N <: PositiveT, M <: NatT]
object NatDivides:
  given [N <: NatT]: NatDivides[Succ[N], _0]()
  given [N <: NatT, M <: NatT] (using NatRemainder[Succ[M], Succ[N]] =:= _0): NatDivides[Succ[N], Succ[M]]()

/**
 * Type representing that one int divides another
 *
 * @tparam J the divisor
 * @tparam I the dividend
 */
trait Divides[J <: IntT, I <: IntT]
object Divides:
  given [J <: NatT, I <: NatT] (using NatDivides[Succ[J], I]): Divides[Succ[J], I]()
  given [J <: NatT, I <: NatT] (using NatDivides[Succ[J], I]): Divides[Minus[Succ[J]], I]()
  given [J <: NatT, I <: NatT] (using NatDivides[Succ[J], Succ[I]]): Divides[Succ[J], Minus[Succ[I]]]()
  given [J <: NatT, I <: NatT] (using NatDivides[Succ[J], Succ[I]]): Divides[Minus[Succ[J]], Minus[Succ[I]]]()
  given [J <: NonZeroIntT, I <: IntT, K <: IntT] (using Divides[J, I]): Divides[J, Prod[K, I]]()
