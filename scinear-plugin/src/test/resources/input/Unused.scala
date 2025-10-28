import scinear.Linear

class Box(val value: Int) extends Linear

/** Don't allow a linear object to be never referenced.
  */
trait Unused {
  def defAndNotUse = {
    val box: Box = // error: LinearTypes
      Box(42)
  }

  def defAndNeverUseMultiple = {
    val x = Box(6) // error: LinearTypes
    val y = Box(7) // error: LinearTypes
    val z = Box(42)
    z
  }

  def functionArg1(
      x: Box,
      y: Box // error: LinearTypes
  ): Int =
    x.value

  def functionArg2(
      x: Int,
      y: Int with Linear // error: LinearTypes
  ): Int =
    42

  def NotUserInnerScope = {
    val x = Box(6)
    val y = Box(7)
    {
      val x = Box(8) // error: not used (captured)
    }
    val z = Box(x.value * y.value)
    z
  }
}
