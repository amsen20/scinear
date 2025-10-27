import scinear.Linear

class Box(val value: Int) extends Linear

trait LinearNode extends Linear

class LinearNil(val name: String, val boxedValue: Box) extends LinearNode
object LinearNil {
  def unapply(node: LinearNil): (String, Box) =
    (node.name, node.boxedValue)
}

class LinearDataNode(val name: String, val next: LinearNode, val boxedValue: Box) extends LinearNode
object LinearDataNode {
  def unapply(node: LinearDataNode): (String, LinearNode, Box) =
    (node.name, node.next, node.boxedValue)
}

/** Allow a iterating through linear linked list
  */
def MapFunction = {
  val node: LinearDataNode =
    LinearDataNode(
      "a",
      LinearDataNode("b", LinearDataNode("c", LinearNil("d", Box(4)), Box(3)), Box(2)),
      Box(1)
    )
  val f: Box => Box = (b: Box) => Box(b.value + 2)

  def map(node: LinearNode, f: Box => Box): LinearNode = {
    node match {
      case LinearDataNode(name, next, boxedValue) =>
        LinearDataNode(name, map(next, f), f(boxedValue))
      case LinearNil(name, boxedValue) =>
        LinearNil(name, f(boxedValue))
    }
  }

  def printAll(node: LinearNode): Unit = {
    node match {
      case LinearNil(name, boxedValue) =>
        println(s"(${name}, ${boxedValue.value})\n")
      case LinearDataNode(name, next, boxedValue) =>
        print(s"(${name}, ${boxedValue.value}) => ")
        printAll(next)
    }
  }

  val mappedNode = map(node, f)
  printAll(mappedNode)
}

class LinearPair[T, U](val x: T, val y: U) extends Linear {
  type A = Int
}
object LinearPair {
  def unapply[T, U](p: LinearPair[T, U]): (T, U) = (p.x, p.y)
}

def gg: (Int, Int) = (1, 2)

@main def main(): Unit =
  MapFunction

  println(gg.getClass().toString())

  def f() = LinearPair(1, 2)

  f() match
    case LinearPair(x, y) =>
      println(s"Matched with x = $x and y = $y")
