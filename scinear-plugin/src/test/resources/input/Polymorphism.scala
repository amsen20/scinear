import scinear.Linear

class Box(val value: Int) extends Linear

// ! IMPORTANT: here when you use the pair, you may not use all fields. It means that when
// ! a linear type becomes garbage, all its fields will become garbage as well.
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
