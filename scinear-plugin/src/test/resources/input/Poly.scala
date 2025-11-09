import scinear.Linear

class Dummy[T](val x: T)
class Box(value: Int) extends Linear

def f[T](x: T): T = x

/** Promote a non-linear polymorphic type when instantiated with a linear type parameter.
  */
def polymorphicPromotion(): Unit = {
  val x: Dummy[Box] = Dummy(Box(42))
  val xx = x
  val xxx = x // error: LinearTypes
  xx
}

/** Don't allow polymorphic function arguments to be linear types.
  */
def polymorphicFunctionArgument(): Unit = {
  val y: Dummy[Dummy[Int]] = Dummy(Dummy(42))
  val z = Box(2)
  val w = f(z) // error: LinearTypes
  w
}

def g[T <: Linear](x: T): T = x

/** Allow polymorphic function arguments with upper bound linear types to be linear.
  */
def boundedPolymorphicFunctionArgument(): Unit = {
  val x = Box(2)
  val y = g(x) // noerror:
  y
}
