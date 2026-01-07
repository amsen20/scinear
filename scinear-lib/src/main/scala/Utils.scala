package scinear.utils

class LinearInt(val value: Int) extends scinear.Linear

/** Enables checking if an option is empty without consuming the linear value inside.
  */
def peekLinearOption[@scinear.HideLinearity T](opt: Option[T]): (Option[T], Boolean) =
  (opt, opt.isEmpty)
