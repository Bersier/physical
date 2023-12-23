package dimensional.dimension

import dimensional.typelevelint.{Diff, IntT, IntQuotient, NonZeroIntT, Prod, Sum}
import dimensional.dimension.Dimensions.Dim

import scala.annotation.targetName

/**
 * Multiplies the two given dims.
 */
@targetName("times") type *[D1, D2] = DimMap2[Sum, D1, D2]

/**
 * Divides the two given dims.
 */
@targetName("over") type /[D1, D2] = DimMap2[Diff, D1, D2]

/**
 * Raises the given dim to the given power.
 *
 * @tparam D the given dim
 * @tparam P the given power
 */
@targetName("toThe") type ~[D, P <: IntT] = DimMap[[Q <: IntT] =>> Prod[Q, P], D]

/**
 * Returns the Nth root of the given dim, assuming it's a valid operation.
 *
 * @tparam D the given dim, whose exponents should be divisible by N
 * @tparam N the root to take
 */
type Root[D, N <: NonZeroIntT] = DimMap[[Z <: IntT] =>> IntQuotient[Z, N], D]

/**
 * Replaces abstract charge by AQ in D.
 *
 * @tparam D  the dim in which the abstract charge is to be replaced
 * @tparam AQ what to replace the abstract charge with
 */
type WithChargeSetTo[D, AQ] = D match
  case Dim[_, _, _, _, _, _, _, _, aQ1, _, _, _, _, _, _, _] => AQ match
    case Dim[_, _, _, _, _, _, _, _, aQ2, _, _, _, _, _, _, _] => SetterHelper[D, AQ, aQ1] match
      case Dim[l, t, p, m, q, n, c, a, _, aP, o1, o2, o3, o4, s, b] => Dim[
        l, t, p, m, q, n, c, a, Prod[aQ1, aQ2], aP, o1, o2, o3, o4, s, b
      ]

/**
 * Replaces abstract potential by AP in D.
 *
 * @tparam D  the dim in which the abstract charge is to be replaced
 * @tparam AP what to replace the abstract charge with
 */
type WithPotentialSetTo[D, AP] = D match
  case Dim[_, _, _, _, _, _, _, _, _, aP1, _, _, _, _, _, _] => AP match
    case Dim[_, _, _, _, _, _, _, _, _, aP2, _, _, _, _, _, _] => SetterHelper[D, AP, aP1] match
      case Dim[l, t, p, m, q, n, c, a, aQ, _, o1, o2, o3, o4, s, b] => Dim[
        l, t, p, m, q, n, c, a, aQ, Prod[aP1, aP2], o1, o2, o3, o4, s, b
      ]

/**
 * Maps the given function over the components of the given dim.
 *
 * @tparam F the given type function
 * @tparam D the given dim
 */
private[dimension] type DimMap[F[_ <: IntT] <: IntT, D] = D match
  case Dim[l, t, p, m, q, n, c, a, aQ, aP, o1, o2, o3, o4, s, b] => Dim[
    F[l], F[t], F[p], F[m], F[q], F[n], F[c], F[a], F[aQ], F[aP], F[o1], F[o2], F[o3], F[o4], F[s], F[b]
  ]

/**
 * Maps the given binary function over the components of the two given dims.
 *
 * @tparam F  the given binary type function
 * @tparam D1 the first given dim
 * @tparam D2 the second given dim
 */
private[dimension] type DimMap2[Op[_ <: IntT, _ <: IntT] <: IntT, D1, D2] = D1 match
  case Dim[l1, t1, p1, m1, q1, n1, c1, a1, aQ1, aP1, o11, o21, o31, o41, s1, b1] => D2 match
    case Dim[l2, t2, p2, m2, q2, n2, c2, a2, aQ2, aP2, o12, o22, o32, o42, s2, b2] => Dim[
      Op[l1, l2], Op[t1, t2], Op[p1, p2], Op[m1, m2], Op[q1, q2], Op[n1, n2], Op[c1, c2], Op[a1, a2], Op[aQ1, aQ2],
      Op[aP1, aP2], Op[o11, o12], Op[o21, o22], Op[o31, o32], Op[o41, o42], Op[s1, s2], Op[b1, b2],
    ]

/**
 * Helper for abstract dimension setters.
 */
private[dimension] type SetterHelper[D, R, M <: IntT] = DimMap2[[I <: IntT, J <: IntT] =>> Sum[I, Prod[M, J]], D, R]
