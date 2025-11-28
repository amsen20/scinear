import scala.language.experimental.captureChecking
import scinear.Linear

class Box(val value: Int) extends Linear
class Capturer[CS^]
class Nested[T]

class Lifetime[OwnersInput^] extends scinear.Linear, caps.Capability {
  type Owners^ = {OwnersInput, this}
  opaque type Key = Object

  def getKey[O^](): Key = {
    new Object
  }
}

/** Don't allow a objects that are capturing a linear value, be used after the linear value is consumed.
  */
trait TypeParamCheck {
  def mentionedInTypeParam = {
    val box: Box^ = Box(42)
    val capturer: Capturer[{box}] = new Capturer[{box}]()
    box.value
    capturer // error: LinearTypes
  }

  def mentionedButNotInTypeParam = {
    val box: Box^ = Box(42)
    val obj: Object^{box} = new Object
    box.value
    obj // noerror:
  }

  def mentionedInNestedTypeParam = {
    val box: Box^ = Box(42)
    val nested: Nested[Object^{box}] = new Nested[Object^{box}]
    box.value
    nested // noerror:
  }

  def mentionedInTypeParamButNotLinear = {
    val o: Object^ = new Object
    val capturer: Capturer[{o}] = new Capturer[{o}]()
    capturer // noerror:
  }

  def mentionedThroughDependentTypes[O1^, O2^, O3^](): Unit = {
    val box: Box^ = Box(42)
    val lf = Lifetime[{box}]()
    val capturer = Capturer[{lf.Owners}]()
    lf
    capturer // error: LinearTypes
    box
  }
}
