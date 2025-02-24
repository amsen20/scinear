import scinear.Linear

case class Box(value: Int) extends Linear

/** Allow a [[Box]] instance to be dereferenced in an if statement.
  */
trait UsageInIf {
  val box: Box = Box(42)
  val condition = true
  if (condition) {
    println(box)
  }
}

/** Allow a [[Box]] instance to be dereferenced in an if-else statement.
  */
trait DoubleUsageInIf {
  val box: Box = Box(42)
  val condition = true
  if (condition) {
    println(box)
  } else {
    println(box)
  }
}

/** Don't allow a [[Box]] instance to be dereferenced after an if-else
  * statement.
  */
trait UsageAfterIf {
  val box: Box = Box(42)
  val condition = true
  if (condition) {
    println(box) // error: LinearTypes
  }
  println(box) // error: LinearTypes
}

/** Don't allow a [[Box]] instance to be dereferenced in a loop.
  */
trait UsageInLoops {
  val box1: Box = Box(42)
  while (true) {
    println(box1) // error: LinearTypes
  }

  val box2: Box = Box(42)
  for (_ <- 1 to 10) {
    println(box2) // error: LinearTypes
  }
}
