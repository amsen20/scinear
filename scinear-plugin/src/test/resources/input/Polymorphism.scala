import scinear.Linear

class Box(val value: Int) extends Linear

/** NOTE: By using a linear pair instance, we might not use one (or both) of its fields. This means
  * that in the tree of linear objects, some nodes might not be used at all.
  */
class LinearPair[T, U](val x: T, val y: U) extends Linear {
  type A = Int
}

object LinearPair {
  def unapply[T, U](p: LinearPair[T, U]): Option[(T, U)] = Some((p.x, p.y))
}

def createLinearPair() = {
  val pair = LinearPair(Box(1), Box(2))
  println(pair.x.value)

  val otherPair = LinearPair(1, 2)
  println(otherPair.y)

  LinearPair(LinearPair(1, 2), LinearPair(3, LinearPair(4, 5))) match {
    case LinearPair(x, y) =>
      println(s"Matched with x = $x and y = $y")
  }
}
