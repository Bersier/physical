package dimensional.typelevelint

/**
 * Type-level Int type
 */
sealed trait IntT derives CanEqual

/**
 * Non-zero IntT type
 */
sealed trait NonZeroIntT extends IntT

/**
 * Type-level Nat type
 */
sealed trait NatT extends IntT

/**
 * Strictly positive NatT type
 */
sealed trait PositiveT extends NatT, NonZeroIntT

/**
 * Type-level 0 type (and term/value)
 */
final case class Zero() extends NatT

/**
 * Type-level non-zero Nat type
 * @param n the predecessor term/value
 * @tparam N the predecessor type
 */
final case class Succ[N <: NatT](n: N) extends NatT, PositiveT

/**
 * Type-level negative Int type
 * @param n the absolute value term/value
 * @tparam N the absolute value type
 */
final case class Minus[N <: PositiveT](n: N) extends NonZeroIntT

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
