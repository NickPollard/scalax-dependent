package nick.dependent

sealed trait Nat

case object Zero extends Nat
case class S[N_ <: Nat]() extends Nat

object Nat {
  type Zero = Zero.type
  type _0 = Zero
  type _1 = S[_0]
  type _2 = S[_1]
  type _3 = S[_2]
  type _4 = S[_3]
  type _5 = S[_4]
}
