// copied and modified from https://github.com/earldouglas/linear-scala

import scinear.Linear

case class Box(value: Int) extends Linear

/** Allow a [[Box]] field to be dereferenced exactly once.
  */
trait FieldUsedOnce {
  val box: Box = Box(42)
  println(box)
}
