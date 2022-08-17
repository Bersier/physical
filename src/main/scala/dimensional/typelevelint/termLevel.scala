package dimensional.typelevelint

/**
 * Sum of two NatTs
 *
 * Note: due to what seems to be a compiler bug, this definition causes a non-exhaustive-match warning to be issued.
 */
def natSum[M <: NatT, N <: NatT](m: M, n: N): NatSum[M, N] = (m, n) match
  case (_, Zero()): (_, Zero) => m
  case (Zero(), _): (Zero, _) => n
  case (Succ(predM), Succ(predN)): (Succ[_], Succ[_]) => Succ(Succ(natSum(predM, predN)))

/**
 * Converts a NatT to an Int.
 */
inline def inlineNatAsInt(inline n: NatT): Int = inline n match
  case _0() => 0
  case Succ(n) => 1 + inlineNatAsInt(n)

/**
 * Converts a NatT to an Int.
 */
def natAsInt(n: NatT): Int = n match
  case _0() => 0
  case Succ(n) => 1 + natAsInt(n)

/**
 * Converts a non-negative Int to a NatT.
 */
def intAsNat(i: Int): NatT =
  assert(i >= 0)
  if i == 0 then _0 else Succ(intAsNat(i - 1))

/**
 * Converts a strictly positive Int to a non-zero NatT.
 */
def positiveIntAsNat(n: Int): Succ[NatT] =
  assert(n > 0)
  Succ(intAsNat(n - 1))

/**
 * Converts an IntT to an Int.
 */
inline def inlineIntTAsInt(inline i: IntT): Int = inline i match
  case Minus(n) => -inlineNatAsInt(n)
  case n: NatT => inlineNatAsInt(n)

/**
 * Converts an IntT to an Int.
 */
def intTAsInt(i: IntT): Int = i match
  case Minus(n) => -natAsInt(n)
  case n: NatT => natAsInt(n)

/**
 * Converts an Int to an IntT.
 */
def intAsIntT(i: Int): IntT = if i < 0 then Minus(positiveIntAsNat(-i)) else intAsNat(i)

/**
 * Difference between two IntTs
 *
 * @param i the minuend
 * @param j the subtrahend
 */
def diff(i: IntT, j: IntT): IntT = intAsIntT(intTAsInt(i) - intTAsInt(j))

/**
 * @param x the base
 * @param i the exponent
 * @return the given Double raised to the given IntT
 */
inline def power(inline x: Double, inline i: IntT): Double = math.pow(x, inlineIntTAsInt(i))

/**
 * @param x the root argument
 * @param i the type of root to take
 * @return the ith root of the given Double
 */
inline def root(inline x: Double, inline i: NonZeroIntT): Double = math.pow(x, 1.0 / inlineIntTAsInt(i))

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
