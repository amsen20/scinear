package scinear

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Types

def isDirectLinear(tpe: Types.Type)(using Context): Boolean =
  // TODO: tpe.isThisTypeOf() --- check if this works
  tpe.baseClasses.exists(_.fullName.toString == "scinear.Linear")

def isFunctionType(tpe: Types.Type)(using Context): Boolean =
  // TODO: Make sure this is the correct way
  tpe.baseClasses.exists(_.fullName.toString.startsWith("scala.Function"))

def isFunctionLinear(tpe: Types.Type)(using Context): Boolean = ???

def isLinear(tpe: Types.Type)(using Context): Boolean =
  // FIXME: Should not allow polymorphic functions to get linear types as type arguments.
  isDirectLinear(tpe) || (
    tpe match
      case Types.AppliedType(
            tycon,
            args
          ) => // promote types with linear arguments to be linear themselves
        // NOTE: If capturing linear values is allowed, then should check if this is curried or not.
        args.exists(isLinear(_)) && !isFunctionType(tycon)
      case _ =>
        false
  )

def isLinear(sym: Symbols.Symbol)(using Context): Boolean =
  isLinear(sym.info)
