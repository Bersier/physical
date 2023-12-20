package dimensional.typelevelint

/**
 * Sum of two NatTs
 */
def natSum[N <: NatT, M <: NatT](n: N, m: M)(using sum: NatSum[N, M]): NatSum[N, M] = sum

/**
 * Converts a NatT to an Int.
 */
inline def inlineNatAsInt(inline n: NatT): Int = inline n match
  case Zero() => 0
  case Succ(n) => 1 + inlineNatAsInt(n)

/**
 * Converts a NatT to an Int.
 */
def natAsInt(n: NatT): Int = n match
  case Zero() => 0
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
def positiveIntAsNat(n: Int): Succ[?] =
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
 * Inline
 *
 * TODO ideally, power would call natPower2 (instead of natPower).
 *      However, I can't get it to compile; Scala 3 seems too buggy at this time.
 *
 * @param x the base
 * @param i the exponent
 * @return the given Double raised to the given IntT
 */
inline def power(inline x: Double, inline i: IntT): Double = inline i match
  case Minus(n) => natPower(1.0 / x, n)
  case n: NatT => natPower(x, n)

/**
 * Uses repeated squaring.
 *
 * @param x the base
 * @param i the exponent
 * @return the given Double raised to the given IntT
 *
 * Note: this definition was used previously.
 */
inline def power2(inline x: Double, inline i: IntT): Double = intPower(x, inlineIntTAsInt(i))

/**
 * Uses repeated squaring.
 *
 * @param x the base
 * @param i the exponent
 * @return the given Double raised to the given Int
 */
private def intPower(x: Double, i: Int): Double =
  if i < 0 then nonNegativeIntPower(1.0 / x, -i, 1.0) else nonNegativeIntPower(x, i, 1.0)

/**
 * Uses repeated squaring.
 *
 * @param x the base
 * @param n the non-negative exponent
 * @return the given Double raised to the given Int
 */
private def nonNegativeIntPower(x: Double, n: Int, acc: Double): Double =
  val newAcc = acc * (if n % 2 == 0 then 1.0 else x)
  if n <= 1 then newAcc else nonNegativeIntPower(x * x, n / 2, newAcc)

/**
 * Inline
 *
 * @param x the base
 * @param n the exponent
 * @return the given Double raised to the given NatT
 */
inline def natPower(x: Double, inline n: NatT): Double = inline n match
  case Zero() => 1.0
  case Succ(predN) => x * natPower(x, predN)

/**
 * Inline. Uses repeated squaring.
 *
 * TODO ideally, natPower2 would call natBinaryQuotient (instead of natBinaryQ and natBinaryR).
 *      However, I can't get it to compile; Scala 3 seems too buggy at this time.
 *
 * Issues: natPower2(2.0, _5) compiles, but natPower2(2.0, _6) doesn't.
 *
 * @param x the base
 * @param n the exponent
 * @return the given Double raised to the given NatT
 */
inline def natPower2(x: Double, inline n: NatT): Double = inline n match
  case Zero() => 1.0
  case Succ(Zero()) => x
  case Succ(Succ(_)) =>
    inline natBinaryR(n) match
      case Zero() => natPower2(x * x, natBinaryQ(n, _0))
      case Succ(_) => x * natPower2(x * x, natBinaryQ(n, _0))

/**
 * @param n the dividend
 * @param acc the accumulator
 * @return the result of division by 2 with remainder and accumulator: (n / 2 + acc, n % 2)
 */
private transparent inline def natBinaryQuotient(inline n: NatT, inline acc: NatT): (NatT, NatT) = inline n match
  case Zero() => (acc, _0)
  case Succ(Zero()) => (acc, _1)
  case Succ(Succ(predPredN)) => natBinaryQuotient(predPredN, Succ(acc))

/**
 * @param n the dividend
 * @param acc the accumulator
 * @return the quotient of division by 2 with remainder and accumulator: n / 2 + acc
 */
private transparent inline def natBinaryQ(inline n: NatT, inline acc: NatT): NatT = inline n match
  case Zero() => acc
  case Succ(Zero()) => acc
  case Succ(Succ(predPredN)) => natBinaryQ(predPredN, Succ(acc))

/**
 * @param n the dividend
 * @return n % 2
 */
private transparent inline def natBinaryR(inline n: NatT): NatT = inline n match
  case Zero() => _0
  case Succ(Zero()) => _1
  case Succ(Succ(predPredN)) => natBinaryR(predPredN)

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
given [PredN <: NatT](using n: Succ[PredN]): Minus[Succ[PredN]] = Minus(n)
