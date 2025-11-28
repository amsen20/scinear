import scinear.Linear

class Box(val value: Int) extends Linear

def tryCatchUsage = {
  val usedInTryBlock: Box = Box(42)
  val usedInOneOfCases: Box = Box(42)
  val usedInAllCases: Box = Box(42)
  val usedInFinally: Box = Box(42)
  val notUsed: Box = Box(42)

  val res =
    try { // error: LinearTypes
      usedInTryBlock
    } catch {
      case e: Exception =>
        usedInOneOfCases
        usedInAllCases
        val definedNotUsed: Box = Box(10) // error: LinearTypes
        ()
      case e: Throwable =>
        usedInAllCases
        ()
    } finally {
      usedInFinally
      val definedNotUsed: Box = Box(10) // error: LinearTypes
      ()
    }

  usedInTryBlock // error: LinearTypes
  usedInOneOfCases // error: LinearTypes
  usedInAllCases // error: LinearTypes
  usedInFinally // error: LinearTypes
  notUsed
  ()
}