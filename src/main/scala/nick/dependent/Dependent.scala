package nick.dependent

/*
trait Foo {
  type T
  def get : T
}

sealed trait Nat
case object Zero extends Nat
case class Succ[N <: Nat] extends Nat

object Nat {
  type _0 = Zero.type
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]

  def plus(a: Nat, b: Nat): Nat = a

  sealed trait Plus[A <: Nat, B <: Nat] {
    type Sum <: Nat
  }

  def dbl[A <: Nat](a: A)(p: Plus[Nat,Nat]): Plus[A,A]#Sum = ???
}

import Nat._

sealed trait Vec[T] {
  type L <: Nat
  type ofLength[N <: Nat] = VecAux[T,N]

  def :<:(t: T): VecAux[T, Succ[this.L]] = Vec.cons(t, this)
  def aux : VecAux[T,L]
}
sealed trait VecAux[T,N <: Nat] extends Vec[T] {
  type L = N
  def aux = this
}
case class VecNil[T] extends Vec[T]#ofLength[_0]
case class VecCons[T, N <: Nat](head: T, tail: Vec[T]#ofLength[N]) extends Vec[T]#ofLength[Succ[N]]

// Existential
trait Vector[T] {
  type L <: Nat
  def value: Vec[T]#ofLength[L]
}
case class Vector_[T,N <: Nat](value: Vec[T]#ofLength[N]) extends Vector[T] { type L = N }

object Vec {
  def nil[T] = VecNil[T]

  def cons[T](a: T, v: Vec[T]): Vec[T]#ofLength[Succ[v.L]] = VecCons(a, v.aux)

  def replace[T](t: T, v: Vec[T]): Vec[T]#ofLength[v.L] = replaceAux(t, v.aux)

  def replaceAux[T,N <: Nat](t: T, v: Vec[T]#ofLength[N]): Vec[T]#ofLength[N] = {
    v match {
      case _:VecNil[T@unchecked] => VecNil[T]
      case c:VecCons[T,N] => VecCons(t, replaceAux(t, c.tail))
    }
  }
  def replace_[T](t: T, v: Vector[T]): Vec[T]#ofLength[v.L] = replaceAux(t, v.value)

  def replace__[T](t: T, v: Vec[T]): Vec[T]#ofLength[v.L] = replaceAux(t, v.aux)
  def map[A,B](f: A => B, v: Vec[A]): Vec[B]#ofLength[v.L] = mapAux(f, v.aux)

  def mapAux[A,B,N <: Nat](f: A => B, v: Vec[A]#ofLength[N]): Vec[B]#ofLength[N] = {
    v match {
      case _:VecNil[A@unchecked] => VecNil[B]
      case VecCons(h,t) => f(h) :<: mapAux(f, t)
      //case VecCons(h,t) => f(h) :<: VecNil[B]
    }
  }

  def map_[A,B](f: A => B, v: Vec[A]): Vec[B]#ofLength[v.L] = {
    v.aux match {
      case VecNil() => VecNil[B]
      case VecCons(h,t) => f(h) :<: mapAux(f, t)
      //case VecCons(h,t) => f(h) :<: VecNil[B]
    }
  }

  //def replace___[T](t: T, v: Vec[T]): Vec[T]#ofLength[v.L] = v.aux match {
    //case _:VecNil[T@unchecked] => VecNil[T]
    //case c:VecCons[T,N] => VecCons(t, replaceAux(t, c.tail))
  //}

  def vn[T](vc: VecNil[T]) : Vec[T]#ofLength[vc.L] = vc

  val a : Vec[Int] = ???
  //val b : VecAux[Int,Succ[Succ[a.L]]] = cons(4, cons(12, a))
  val c : VecAux[Int,Succ[Succ[a.L]]] = 4 :<: 12 :<: a
  val d : VecAux[Int, _0] = nil
  val e : VecAux[Int, _4] = 1 :<: 2 :<: 3 :<: 4 :<: nil
  val f : VecAux[Int, _4] = replace__(12, 1 :<: 2 :<: 3 :<: 4 :<: nil)
  val g : VecAux[Int, _4] = replace_(12, Vector_(1 :<: 2 :<: 3 :<: 4 :<: nil))
}
*/

