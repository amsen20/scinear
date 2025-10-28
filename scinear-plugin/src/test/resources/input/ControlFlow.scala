import scinear.Linear

class Box(val value: Int) extends Linear

/** Allow a linear object to be referenced in an if statement.
  */
def UsageInIf = {
  val box: Box = Box(42)
  val condition = true
  if (condition) {
    println(box)
  } else {
    box
    ()
  }
}

/** Allow a linear object to be referenced in an if-else statement.
  */
def DoubleUsageInIf = {
  val box: Box = Box(42)
  val condition = true
  if (condition) {
    println(box)
  } else {
    println(box)
  }
}

/** Don't allow a linear object to be referenced after an if-else statement.
  */
def UsageAfterIf = {
  val box: Box = Box(42)
  val condition = true
  if (condition) {
    println(box)
  } else {
    box
    ()
  }
  println(box) // error: LinearTypes
}

/** Don't allow a linear object to be referenced in a loop.
  */
def UsageInLoops = {
  val box1: Box = Box(42) // error: LinearTypes
  while (true) {
    println(box1) // error: LinearTypes
  }

  val box2: Box = Box(42) // error: LinearTypes
  for (_ <- 1 to 10) {
    println(box2) // error: LinearTypes
  }
}