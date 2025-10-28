// copied and modified from https://github.com/earldouglas/linear-scala

import scinear.Linear

class Box(val value: Int) extends Linear

/** Don't allow a linear object be referenced more than once.
  */
def MultipleUse = {
  def case0 = {
    val box: Box = Box(42)
    println(box)
    println(box) // error: LinearTypes
  }

  def case1 = {
    val x = Box(6)
    val y = Box(x.value + 1)
    val z = Box(x.value * y.value) // error: LinearTypes
    z
  }

  def case2 = {
    val x = Box(6)
    val y = Box(7)
    val z = Box(x.value * y.value)
    (x, y, z) // error: LinearTypes
  }
}
