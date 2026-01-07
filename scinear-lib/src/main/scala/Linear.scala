package scinear

/** Used to enable passing linear types as non-linear type parameters to polymorphic functions. Be
  * cautious when using this trait, as it may lead to linearity violations. This should be only used
  * by the libraries internal implementations.
  *
  * TODO: At least warn when this is used. A better approach would be through including
  * `scinear.unsafe` to enable this trait.
  */
sealed class HideLinearity extends annotation.StaticAnnotation

/** TODO: Make this an annotation.
  *
  * TODO: Either change `UnsafeBase` name to something more descriptive, like `BasicType`, or make
  * both of them annotations.
  */
trait Linear:
  /** A method to consume a linear value. Useful when need to use a linear value but has no actual
    * use for it.
    *
    * Call this instead of leaving it in a single expression to avoid compiler warnings.
    */
  def consume(): Unit = ()
end Linear
