package dimensional.additionalunits

import dimensional.dimension.Dimensions.{*, given}
import dimensional.typelevelint.*

import scala.language.implicitConversions

inline def nano[
  L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
  O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, O5 <: IntT, O6 <: IntT,
](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
  L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
] = x * 1e-9

inline def micro[
  L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
  O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, O5 <: IntT, O6 <: IntT,
](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
  L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
] = x * 1e-6

inline def milli[
  L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
  O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, O5 <: IntT, O6 <: IntT,
](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
  L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
] = x * 1e-3

inline def centi[
  L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
  O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, O5 <: IntT, O6 <: IntT,
](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
  L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
] = x * 1e-2

inline def deci[
  L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
  O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, O5 <: IntT, O6 <: IntT,
](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
  L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
] = x * 1e-1

inline def kilo[
  L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
  O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, O5 <: IntT, O6 <: IntT,
](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
  L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
] = x * 1e3

inline def mega[
  L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
  O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, O5 <: IntT, O6 <: IntT,
](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
  L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
] = x * 1e6

inline def giga[
  L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
  O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, O5 <: IntT, O6 <: IntT,
](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
  L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
] = x * 1e9

inline def tera[
  L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
  O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, O5 <: IntT, O6 <: IntT,
](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
  L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
] = x * 1e12

inline def peta[
  L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
  O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, O5 <: IntT, O6 <: IntT,
](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
  L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
] = x * 1e15

inline def kibi[
  L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
  O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, O5 <: IntT, O6 <: IntT,
](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
  L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
] = x * 1024

inline def mebi[
  L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
  O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, O5 <: IntT, O6 <: IntT,
](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
  L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
] = x * math.pow(1024, 2)

inline def gibi[
  L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
  O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, O5 <: IntT, O6 <: IntT,
](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
  L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
] = x * math.pow(1024, 3)

inline def tebi[
  L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
  O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, O5 <: IntT, O6 <: IntT,
](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
  L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
] = x * math.pow(1024, 4)

inline def pebi[
  L <: IntT, T <: IntT, P <: IntT, M <: IntT, Q <: IntT, N <: IntT, C <: IntT, A <: IntT, AQ <: IntT, AP <: IntT,
  O1 <: IntT, O2 <: IntT, O3 <: IntT, O4 <: IntT, O5 <: IntT, O6 <: IntT,
](x: Dim[L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6]): Dim[
  L, T, P, M, Q, N, C, A, AQ, AP, O1, O2, O3, O4, O5, O6
] = x * math.pow(1024, 5)

