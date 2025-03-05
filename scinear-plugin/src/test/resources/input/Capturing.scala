import scinear.Linear

case class Box(value: Int) extends Linear

/** Don't allow a [[Box]] instance to be captured by a closure.
  */
def Capturing = {
  val box: Box = Box(42) // error:
  val f: () => Unit = () => println(box) // error: LinearTypes
}