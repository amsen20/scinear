import scinear.Linear

class Box(val value: Int) extends Linear

trait LinearNode extends Linear

class LinearNil(val name: String, val boxedValue: Box) extends LinearNode
object LinearNil {
  def unapply(node: LinearNil): Option[(String, Box)] =
    Some((node.name, node.boxedValue))
}

class LinearDataNode(val name: String, val next: LinearNode, val boxedValue: Box) extends LinearNode
object LinearDataNode {
  def unapply(node: LinearDataNode): Option[(String, LinearNode, Box)] =
    Some((node.name, node.next, node.boxedValue))
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
  val f: Box => Box = (b: Box) => Box(b.value + 1)

  def map(node: LinearNode, f: Box => Box): LinearNode = {
    node match {
      case LinearDataNode(name, next, boxedValue) =>
        LinearDataNode(name, map(next, f), f(boxedValue))
      case l: LinearNil => l
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

/** Don't allow a non-linear class has a field of linear type
  */
class A1(val box: Box) extends Linear
class B(val box: Box) // error: LinearTypes

/** Don't allow linear types to have methods Methods can be implemented by defining functions
  * getting the linear type as an argument (self)
  */
class A2(val name: String) extends Linear {
  def print(): Unit = { // error: LinearTypes
    println(this.name)
  }
}

/** Don't allow a linear type to mention this during its initialization
  */
class A3(val name: String) extends Linear {
  val self = this // error: LinearTypes
}

/** Don't allow using a linear type twice during initialization
  */
class A4(val box: Box) extends Linear {
  val a = box.value // error: LinearTypes
  val b = box.value // error: LinearTypes
}

/** Don't allow nested class/trait/object definition for linear types (for now).
  */
class A6(val box: Box) extends Linear {
  class B // error: LinearTypes
  trait C // error: LinearTypes
  object D // error: LinearTypes
}

/** Allow linear types to have fields initialization
  */
class A7(val box: Box) extends Linear {
  val a = 1 // noerror
}
