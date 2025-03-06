import scinear.Linear

class Dummy[T](val x: T)
class Box(value: Int) extends Linear

def f[T](x: T): T = x

/** Don't allow a polymorphic type to be initialized with a linear type.
  */
def UnusedPoly = {
  val x: Dummy[Box] = Dummy(Box(42)) // error: LinearTypes
  val y: Dummy[Dummy[Int]] = Dummy(Dummy(42))
  val z = Box(2)
  val w = f(z) // error: LinearTypes
  w
}