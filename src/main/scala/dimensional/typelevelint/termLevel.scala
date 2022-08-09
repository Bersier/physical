package dimensional.typelevelint

// TODO can the signature somehow be refined to "natSum[X <: NatT, Y <: NatT](x: X, y: Y): NatSum[X, Y]"?
/**
 * Sum of two NatTs
 */
def natSum(m: NatT, n: NatT): NatT = (m, n) match
  case (_, Zero()) => m
  case (Zero(), _) => n
  case (Succ(predM), Succ(predN)) => Succ(Succ(natSum(predM, predN)))

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
