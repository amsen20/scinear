package scinear

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Types

def isDirectLinear(tpe: Types.Type)(using Context): Boolean =
  // TODO: use `requiredClass` instead of string comparison.
  tpe.baseClasses.exists(_.fullName.toString == "scinear.Linear")

def doesHideLinearity(sym: Symbols.Symbol)(using Context): Boolean =
  sym.annotations.exists(annot => annot.symbol.fullName.toString == "scinear.HideLinearity")

def isFunctionType(tpe: Types.Type)(using Context): Boolean =
  // TODO: Make sure this is the correct way
  tpe.baseClasses.exists(_.fullName.toString.startsWith("scala.Function"))

def isFunctionLinear(tpe: Types.Type)(using Context): Boolean = ???

def isLinear(tpe: Types.Type)(using Context): Boolean =
  // TODO: Only allow `Option` and `Tuple` to be promoted to linear.
  // TODO: Do not allow other types to have linear type parameter unless they are marking it as `@HideLinearity`.
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
