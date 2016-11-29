// Bools as singleton types
sealed trait Bool {
  type And[T <: Bool] <: Bool
  def and[T <: Bool](b : T): And[T]
}
case object True extends Bool {
  type And[T <: Bool] = T
  def and[T <: Bool](b : T): And[T] = b
}
case object False extends Bool {
  type And[T <: Bool] = False.type
  def and[T <: Bool](b : T): And[T] = False
}

sealed trait SBool {
  type T <: Bool
  def aux: SBoolAux[T]
  def value: T
}
sealed trait SBoolAux[U <: Bool] extends SBool {
  type T = U
}
case class STrue() extends SBoolAux[True.type] {
  def value : True.type = True
  def aux = this
}
case class SFalse() extends SBoolAux[False.type] {
  def value : False.type = False
  def aux = this
}

object SBool {
 type &&[A <: Bool, B <: Bool] = A#And[B]

 def and(b: SBool, c: SBool): b.T && c.T = {
   def andAux[B <: Bool, C <: Bool](b: SBoolAux[B], c: SBoolAux[C]): B && C = b.value and c.value

   andAux(b.aux,c.aux)
 }
}
