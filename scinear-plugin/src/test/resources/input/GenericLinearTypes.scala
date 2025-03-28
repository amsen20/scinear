import scinear.Linear

class Box(val value: Int) extends Linear

class LinearPair[T, U](val x: T, val y: U) extends Linear {
  type A = Int
}

def createLinearPair() = {
  val pair = LinearPair(Box(1), Box(2))
  println(pair.x.value)

  val otherPair = LinearPair(1, 2)
  println(otherPair.y)
}