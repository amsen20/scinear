// copied and modified from https://github.com/earldouglas/linear-scala

import scinear.Linear

class Box(val value: Int) extends Linear

/** Allow a linear object field to be referenced exactly once.
  */
def FieldUsedOnce = {
  val box: Box = Box(42)
  println(box)
}

def UseUsingConsume = {
  val box: Box = Box(42)
  box.consume()
}
