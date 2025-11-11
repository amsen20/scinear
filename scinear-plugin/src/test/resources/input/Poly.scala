import scinear.HideLinearity
import scinear.Linear

class Holder[T](val x: T)
class Box(value: Int) extends Linear

def f[T](x: T): T = x

/** Promote a non-linear polymorphic type when instantiated with a linear type parameter.
  */
def polymorphicPromotion(): Unit = {
  val x: Holder[Box] = Holder(Box(42))
  val xx = x
  val xxx = x // error: LinearTypes
  xx
}

/** Don't allow polymorphic function arguments to be linear types.
  */
def polymorphicFunctionArgument(): Unit = {
  val y: Holder[Holder[Int]] = Holder(Holder(42))
  val z = Box(2)
  val w = f(z) // error: LinearTypes
  w
}

def g[T](x: T): T = x
def gLinear[T <: Linear](x: T): T = x
def justTypeParam[T]: Unit = ()
def justTypeParamHider[@HideLinearity T]: Unit = ()
def justTypeParamLinear[T <: Linear]: Unit = ()

/** Allow polymorphic function arguments with upper bound linear types to be linear.
  */
def boundedPolymorphicFunctionArgument(): Unit = {
  justTypeParam[Box] // error: LinearTypes
  justTypeParamHider[Box] // noerror:
  justTypeParamLinear[Box] // noerror:
  val x1 = Box(1)
  val y1 = g(x1) // error: LinearTypes
  y1
  val x2 = Box(2)
  val y2 = gLinear(x2) // noerror:
  y2
}

def holderAccessor[T](holder: Holder[T]): T = holder.x
def holderAccessorLinear[T <: Linear](holder: Holder[T]): T = holder.x

def boundedPolymorphicFunctionArgumentThroughHolder(): Unit = {
  holderAccessor(Holder(Box(42))) // error: LinearTypes
  holderAccessorLinear(Holder(Box(42)))
}
