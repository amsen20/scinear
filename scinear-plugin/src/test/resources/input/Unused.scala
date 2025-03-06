// copied and modified from https://github.com/earldouglas/linear-scala

import scinear.Linear

class Box(val value: Int) extends Linear

/** Don't allow a [[Box]] field to be created but never dereferenced.
  */
def UnusedField = {
  val box: Box = // error: LinearTypes
    Box(42)
}

/** Don't allow a [[Box]] parameter to be declared but never dereferenced.
  */
trait UnusedParameter {
  def foo(
      x: Box,
      y: Box // error: LinearTypes
  ): Int =
    x.value
}

/** I believe the method definition is fine. A linear value is created and returned (like new).
  */
// trait UnusedMethod {
//   def foo(): Box =
//     Box(42)
// }

/** Don't allow a [[Box]] value to be created but never dereferenced.
  */
trait UnusedValue {
  def foo(): Unit = {
    val x: Box = Box(42) // error: LinearTypes
  }
}

/** Don't allow a [[Box]] binding in a for comprehension to be created but never dereferenced.
  */
def UnusedBinding = {
  val x = Box(6) // error: LinearTypes
  val y = Box(7) // error: LinearTypes
  val z = Box(42)
  z
}

/** Don't allow a field with a [[Linear]] structural type to be created but never dereferenced. This is not OK anymore,
  * because casting non-linear types to linear type causes violation of linear properties.
  */
// def UnusedFieldWithStructuralType = {
//   val x: Int with Linear = // noerror: LinearTypes
//     42.asInstanceOf[Int with Linear]
// }

/** Don't allow a parameter with a [[Linear]] structural type to be declared but never dereferenced.
  */
trait UnusedParameterWithStructuralType {
  def foo(
      x: Int,
      y: Int with Linear // error: LinearTypes
  ): Int =
    42
}

/** Don't allow a [[Box]] binding in a for comprehension to be shadowed but never dereferenced.
  */
def UnusedShadow = {
  val x = Box(6)
  val y = Box(7)
  {
    val x = Box(8) // error: not used (captured)
  }
  val z = Box(x.value * y.value)
  z
}