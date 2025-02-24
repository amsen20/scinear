// copied and modified from https://github.com/earldouglas/linear-scala

import scinear.Linear

case class Box(value: Int) extends Linear

/** Don't allow a [[Box]] field to be dereferenced more than once.
  */
trait FieldUsedTwice {
  val box: Box = Box(42)
  println(box) // error: LinearTypes
  println(box) // error: LinearTypes
}

/** Don't allow a [[Box]] binding in a for comprehension to be dereferenced more
  * than once.
  */
trait BindingUsedTwice {
  for {
    x <- Some(Box(6))
    y <- Some(Box(x.value + 1)) // error: LinearTypes
    z <- Some(Box(x.value * y.value)) // error: LinearTypes
  } yield z

  for {
    x <- Some(Box(6))
    y <- Some(Box(7))
    z <- Some(Box(x.value * y.value)) // error: LinearTypes
  } yield (x, y, z) // error: LinearTypes
}

/** Don't allow a field with a [[Linear]] structural type to be dereferenced
  * more than once.
  */
trait FieldWithStructuralTypeUsedTwice {
  val x: Int with Linear = 42.asInstanceOf[Int with Linear]
  println(x) // error: LinearTypes
  println(x) // error: LinearTypes
}
