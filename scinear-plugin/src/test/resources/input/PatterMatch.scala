import scinear.Linear

import scala.language.experimental.captureChecking

class Box(value: Int) extends Linear

def f(): (Box, Box) = {
  val b1 = new Box(1)
  val b2 = new Box(2)
  (b1, b2)
}

def g(): Box = {
  val (b1, b2) = f()
  b1
  b2
  b1 // error: LinearTypes
}
