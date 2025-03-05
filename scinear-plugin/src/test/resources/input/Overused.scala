// copied and modified from https://github.com/earldouglas/linear-scala

import scinear.Linear

case class Box(value: Int) extends Linear

/** Don't allow a [[Box]] field to be dereferenced more than once.
  */
def FieldUsedTwice = {
  val box: Box = Box(42)
  println(box)
  println(box) // error: LinearTypes
}

/** Don't allow a [[Box]] binding in a for comprehension to be dereferenced more than once.
  */
def BindingUsedTwice = {
  def case1 = {
    val x = Box(6)
    val y = Box(x.value + 1)
    val z = Box(x.value * y.value) // error: LinearTypes
    z
  }

  def case2 = {
    val x = Box(6)
    val y = Box(7)
    val z = Box(x.value * y.value)
    (x, y, z) // error: LinearTypes
  }
}

/** Don't allow a field with a [[Linear]] structural type to be dereferenced more than once.
  */
def FieldWithStructuralTypeUsedTwice = {
  val x: Int with Linear = 42.asInstanceOf[Int with Linear]
  println(x)
  println(x) // error: LinearTypes
}
