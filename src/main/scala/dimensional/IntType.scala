package dimensional

object IntType:
  sealed trait IntT
  sealed trait NonZeroIntT extends IntT
  sealed trait NatT extends IntT
  final case class Zero() extends NatT
  final case class Succ[+N <: NatT](n: N) extends NatT with NonZeroIntT
  final case class Minus[+N <: Succ[NatT]](n: N) extends NonZeroIntT

  sealed trait BoolT
  final case class BotT() extends BoolT
  final case class TopT() extends BoolT

  given Conversion[Int, IntT] with
    transparent inline def apply(i: Int): IntT =
      if i < 0 then Minus(positive(-i))
      else if i == 0 then Zero()
      else positive(i)

    private[this] def positive(i: Int): Succ[NatT] = if i == 1
      then Succ(Zero())
      else Succ(positive(i - 1))

  type NatSum[X <: NatT, Y <: NatT] <: NatT = (X, Y) match
    case (_, `_0`) => X
    case (`_0`, _) => Y
    case (Succ[x], Succ[y]) => Succ[Succ[NatSum[x, y]]]

  type First[P] = P match
    case (x, _) => x

  type Second[P] = P match
    case (_, y) => y

  type NatDiff[X <: NatT, Y <: NatT] <: IntT = (X, Y) match
    case (_, `_0`) => X
    case (`_0`, _) => Minus[Y]
    case (Succ[x], Succ[y]) => NatDiff[x, y]

  type NatLessThan[X <: NatT, Y <: NatT] = NatDiff[X, Y] match
    case Minus[_] => TopT
    case _ => BotT

  trait NatIsLessThan[X <: NatT, Y <: NatT]
  object NatIsLessThan:
    given[X <: NatT, Y <: NatT](using NatLessThan[X, Y] =:= TopT): NatIsLessThan[X, Y]()

  type NatQuotientHelper[X <: NatT, Y <: Succ[NatT], QAcc <: NatT] = NatDiff[X, Y] match
    case Minus[_] => (QAcc, X)
    case `_0` => (Succ[QAcc], _0)
    case Succ[x] => NatQuotientHelper[Succ[x], Y, Succ[QAcc]]

  type NatQuotient[X <: NatT, Y <: Succ[NatT]] = Y match
    case `_1` => (X, _0)
    case _ => NatQuotientHelper[X, Y, _0]

  type NatQuotientFloor[X <: NatT, Y <: Succ[NatT]] = First[NatQuotient[X, Y]]

  type NatRemainder[X <: NatT, Y <: Succ[NatT]] = Second[NatQuotient[X, Y]]

  type IntQuotient[X <: IntT, Y <: NonZeroIntT] = (X, Y) match
    case (_, `_1`) => (X, _0)
    case (Minus[x], Minus[y]) => NatQuotientFloor[x, y]
    case (_, Minus[y]) => Minus[NatQuotientFloor[X, y]]
    case (Minus[x], _) => Minus[NatQuotientFloor[x, Y]]
    case _ => NatQuotientFloor[X, Y]

  type Neg[X <: IntT] <: IntT = X match
    case _0 => _0
    case Minus[x] => x
    case _ => Minus[X]

  type Sum[X <: IntT, Y <: IntT] <: IntT = (X, Y) match
    case (_, `_0`) => X
    case (Minus[x], Minus[y]) => Minus[NatSum[x, y]]
    case (_, Minus[y]) => NatDiff[X, y]
    case (Minus[x], _) => NatDiff[Y, x]
    case _ => NatSum[X, Y]

  type Diff[X <: IntT, Y <: IntT] = Sum[X, Neg[Y]]

  type NatProd[X <: NatT, Y <: NatT] <: NatT = (X, Y) match
    case (_, `_0`) => _0
    case (_, `_1`) => X
    case (`_0`, _) => _0
    case (`_1`, _) => Y
    case (Succ[x], Succ[y]) => NatSum[NatSum[_1, NatSum[x, y]], NatProd[x, y]]

  type Prod[X <: IntT, Y <: IntT] <: IntT = (X, Y) match
    case (_, `_0`) => _0
    case (_, `_1`) => X
    case (Minus[x], Minus[y]) => NatProd[x, y]
    case (_, Minus[y]) => Minus[NatProd[X, y]]
    case (Minus[x], _) => Minus[NatProd[x, Y]]
    case _ => NatProd[X, Y]

  trait NatDivides[X <: Succ[NatT], Y <: NatT]
  object NatDivides:
    given [X <: Succ[NatT]]: NatDivides[X, _0]()
    given [X <: Succ[NatT], Y <: Succ[NatT]](using NatRemainder[Y, X] =:= _0): NatDivides[X, Y]()

  trait Divides[X <: IntT, Y <: IntT]
  object Divides:
    given [X <: Succ[NatT], Y <: NatT](using NatDivides[X, Y]): Divides[X, Y]()
    given [X <: Succ[NatT], Y <: NatT](using NatDivides[X, Y]): Divides[Minus[X], Y]()
    given [X <: Succ[NatT], Y <: Succ[NatT]](using NatDivides[X, Y]): Divides[X, Minus[Y]]()
    given [X <: Succ[NatT], Y <: Succ[NatT]](using NatDivides[X, Y]): Divides[Minus[X], Minus[Y]]()

  def natAsInt(x: NatT): Int = x match
    case _0() => 0
    case Succ(n) => 1 + natAsInt(n)

  def natPower(x: Double, y: NatT): Double = math.pow(x, natAsInt(y))

  def power(x: Double, y: IntT): Double = y match
    case Minus(n) => math.pow(natPower(x, n), -1)
    case n: NatT => natPower(x, n)

  def natRoot(x: Double, y: NatT): Double = math.pow(x, 1.0 / natAsInt(y))

  def root(x: Double, y: IntT): Double = y match
    case Minus(n) => math.pow(natRoot(x, n), -1)
    case n: NatT => natRoot(x, n)

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

  given Zero = _0
  given [N <: NatT](using n: N): Succ[N] = Succ(n)
  given [N <: Succ[NatT]](using n: N): Minus[N] = Minus(n)
end IntType
