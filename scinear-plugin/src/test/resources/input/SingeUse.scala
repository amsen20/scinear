// copied and modified from https://github.com/earldouglas/linear-scala

import scinear.Linear

class Box(val value: Int) extends Linear

/** Allow a [[Box]] field to be dereferenced exactly once.
  */
def FieldUsedOnce = {
  val box: Box = Box(42)
  println(box)
}