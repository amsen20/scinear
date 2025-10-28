import scinear.Linear

class Box(val value: Int) extends Linear

/** Don't allow a closure to access a linear object through its environment.
  */
def Closure = {
  val box: Box = Box(42) // error:
  val f: () => Unit = () => println(box) // error: LinearTypes
}