import scala.language.experimental.captureChecking
import scinear.Linear

class Box(val value: Int) extends Linear
class Capturer[CS^]
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
}