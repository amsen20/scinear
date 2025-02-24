import scinear.Linear

case class Box(value: Int) extends Linear

sealed trait LinearLinkedList extends Linear
case class Nil() extends LinearLinkedList
case class Node(name: String, next: Node, boxedValue: Box)
    extends LinearLinkedList

/** Allow a iterating through linear linked list
  */
trait MapFunction {
  val node: Node = Node("a", Node("b", Node("c", null, Box(3)), Box(2)), Box(1))
  val f: Box => Box = (b: Box) => Box(b.value + 1)

  def map(node: Node, f: Box => Box): Node = {
    node match {
      case Nil() => Nil()
      case Node(name, next, boxedValue) =>
        Node(name, map(next, f), f(boxedValue))
    }
  }

  def printAll(node: Node): Unit = {
    node match {
      case Nil() =>
        println("Nil")
      case Node(name, next, boxedValue) =>
        print(s"(${name}, ${boxedValue.value}) => ")
        printAll(next)
    }
  }

  val mappedNode = map(node, f)
  printAll(mappedNode)
}

/** Don't allow a non-linear class has a field of linear type
  */
trait LinearField {
  class A(val box: Box) extends Linear
  class B(val box: Box) // error: LinearTypes
}

/** Don't allow linear types to have methods Methods can be implemented by
  * defining functions getting the linear type as an argument (self)
  */
trait AccessingFieldsAndMethods {
  class A(val name: String) extends Linear {
    def print(): Unit = { // error: LinearTypes
      println(this.name)
    }
  }
}

/** Don't allow a linear type to mention this during its initialization
  */
trait ThisInInitialization {
  class A(val name: String) extends Linear {
    val self = this // error: LinearTypes
  }
}

/** Don't allow using a linear type twice during initialization
  */
trait LinearTypeUsedTwice {
  class A(val box: Box) extends Linear {
    val a = box.value // error: LinearTypes
    val b = box.value // error: LinearTypes
  }
}

/** Don't allow objects of a linear type
  */
trait ObjectOfLinearType {
  object A extends Linear // error: LinearTypes
}

/** Don't allow nested class/trait/object definition for linear types (for now).
  */
trait NestedDefinition {
  class A(val box: Box) extends Linear {
    class B // error: LinearTypes
    trait C // error: LinearTypes
    object D // error: LinearTypes
  }
}
