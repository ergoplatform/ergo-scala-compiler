package org.ergoplatform.compiler.compilation

import sigmastate.{
  SBoolean,
  SByte,
  SCollectionType,
  SFunc,
  SInt,
  SLong,
  SOption,
  SShort,
  SSigmaProp,
  STuple,
  SType,
  Values
}
import org.ergoplatform.compiler.compilation.util.RichContext
import sigmastate.Values.{SValue, Value}

import scala.reflect.macros.whitebox

trait Types {

  val c: whitebox.Context
  import c.universe.{Block => _, Constant => _, Function => _, Ident => _, If => _, _}
  import c.universe.definitions._

  // TODO: extract
  protected case class ScalaTree(tree: Tree) extends SValue {
    override def companion: Values.ValueCompanion = ???
    override def tpe: SType                       = tpeToSType(tree.tpe)
    override def opType: SFunc                    = Value.notSupportedError(this, "opType")
  }

  protected def isTypeTuple(tpe: Type) =
    tpe.typeSymbol.fullName startsWith "scala.Tuple"

  protected def isTypeColl(tpe: Type) =
    tpe.typeSymbol.fullName startsWith "special.collection.Coll"

  protected def isTypeBox(tpe: Type) =
    tpe.typeSymbol.fullName startsWith "special.sigma.Box"

  protected def isTypeOption(tpe: Type) =
    tpe.typeSymbol.fullName startsWith "scala.Option"

  protected def isTypeSigmaProp(tpe: Type) =
    tpe.typeSymbol.fullName startsWith "special.sigma.SigmaProp"

  protected def isTypeBoolean(tpe: Type) = tpe == BooleanTpe

  protected def tpeToSType(tpe: Type): SType = tpe.widen match {
    case BooleanTpe                                      => SBoolean
    case ByteTpe                                         => SByte
    case ShortTpe                                        => SShort
    case IntTpe                                          => SInt
    case LongTpe                                         => SLong
    case t @ TypeRef(_, _, List(arg)) if isTypeOption(t) => SOption(tpeToSType(arg))
    case t @ TypeRef(_, _, List(arg)) if isTypeColl(t)   => SCollectionType(tpeToSType(arg))
    case t @ TypeRef(_, _, targs) if isTypeTuple(t) =>
      STuple(targs.map(tpeToSType).toIndexedSeq)
    case t @ TypeRef(_, _, targs) if isTypeSigmaProp(t) => SSigmaProp
    case v @ _                                          => c.fail(s"cannot convert tpe $v to SType")
  }

}
