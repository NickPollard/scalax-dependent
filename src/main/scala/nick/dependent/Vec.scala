package nick.dependent

import Nat._

sealed trait Vec[T] {
  type Length <: Nat

  def :<:(t: T): VecN[T,S[this.Length]] = new :<:(t, aux)
  def aux: VecN[T,Length]
}

sealed trait VecN[T,N <: Nat] extends Vec[T] {
  type Length = N
  def aux = this
}

case class VecNil[T]() extends VecN[T,_0]
case class :<:[T, N <: Nat](head: T, tail: VecN[T,N]) extends VecN[T,S[N]]

object Vec {
  def nil[T] = VecNil[T]

  def map[A,B](f: A => B, v: Vec[A]): VecN[B,v.Length] = mapN(f, v.aux)

  def mapN[A,B,N <: Nat](f: A => B, v: VecN[A,N]): VecN[B,N] = {
    v match {
      case VecNil() => VecNil()
      case h :<: t => f(h) :<: mapN(f, t)
    }
  }

  def foo(i: Int):Vec[Int] = {
    if (i < 0) 1 :<: nil
    else 2 :<: 3 :<: nil
  }

  val a : Vec[Int] = foo(1)
  val a0 = map((i : Int) => i + 1, a)
  val a1 : VecN[Int,a.Length] = a0
  //val a2 : VecN[Int,_2] = a0

  val foo : Vec[Int] = ???
  val good : VecN[Int,S[foo.Length]] = 1 :<: foo
  //val bad : VecN[Int,S[foo.Length]] = 1 :<: 2 :<: foo

  val c : VecN[Int,S[S[a.Length]]] = 4 :<: 12 :<: a
  val d : VecN[Int,_0] = nil
  val e : VecN[Int,_4] = 1 :<: 2 :<: 3 :<: 4 :<: nil
  val h : Vec[String] = ???
  val i : VecN[Int,h.Length] = map((s: String) => s.length(), h)
  val i0 = map((s: String) => s.length(), h)
  val i1 : VecN[Int,h.Length] = i0

  val t = 1 :<: 2 :<: nil
  val t1 = map((i : Int) => i + 1, t)
  val t2 : VecN[Int,_2] = t1
}
