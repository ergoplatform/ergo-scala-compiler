package org.ergoplatform.compiler.compilation

import org.ergoplatform.ErgoBox.RegisterId
import org.ergoplatform.{Height, Outputs, Self}
import org.ergoplatform.compiler.compilation.util.RichContext
import sigmastate.{
  BinAnd,
  BinOr,
  BoolToSigmaProp,
  CreateProveDlog,
  EQ,
  GE,
  GT,
  Relation,
  SBoolean,
  SBox,
  SByte,
  SCollection,
  SCollectionType,
  SFunc,
  SGenericType,
  SInt,
  SLong,
  SOption,
  SShort,
  SSigmaProp,
  STuple,
  SType,
  STypeVar,
  SigmaOr,
  SigmaTransformer,
  Values
}
import sigmastate.Values.{
  BlockItem,
  BlockValue,
  BoolValue,
  IntConstant,
  NotReadyValue,
  NotReadyValueBoolean,
  SValue,
  SigmaPropValue,
  ValDef,
  ValUse,
  Value
}
import sigmastate.lang.Terms.ValueOps
import sigmastate.utxo.{
  ByIndex,
  Extract,
  ExtractBytes,
  ExtractId,
  ExtractRegisterAs,
  ExtractScriptBytes,
  OptionGet,
  OptionIsDefined,
  SelectField,
  SigmaPropBytes,
  SizeOf,
  Transformer
}

import scala.reflect.macros.whitebox

trait Liftables extends Types {
  val c: whitebox.Context
  import c.universe.{
    Block => _,
    Constant => _,
    Function => _,
    Ident => _,
    If => _,
    ValDef => _,
    Transformer => _,
    _
  }

  private val epack  = q"org.ergoplatform"
  private val svpack = q"sigmastate.Values"
  private val spack  = q"sigmastate"
  private val supack = q"sigmastate.utxo"

  implicit val sTypeVarLiftable = Liftable[STypeVar] {
    case STypeVar(name) => q"$spack.STypeVar($name)"
  }

  implicit val blockItemLiftable = Liftable[BlockItem] {
    case ValDef(id, tpeArgs, rhs) => q"$svpack.ValDef($id, ..$tpeArgs, $rhs)"
  }

  implicit val registerIdLiftable = Liftable[RegisterId] {
    case ri: RegisterId => q"$epack.ErgoBox.registerByIndex(${ri.asIndex})"
    case v @ _          => c.fail(s"no Liftable for RegisterId: $v")
  }

//  implicit def extractLiftable[T <: SType] = Liftable[Extract[[T]]] {
//    case ExtractRegisterAs(b, r, tpe) => q"$supack.ExtractRegisterAs($b, $r, $tpe)"
//    case v @ _                        => c.fail(s"no Liftable for Extract: $v")
//  }

//  implicit def valUseLiftable[T <: SType] = Liftable[ValUse[T]] {
//    case ValUse(i, tpe) => q"$spack.ValUse($i, $tpe)"
//  }

  implicit def astLiftable[T <: SType]: Liftable[Value[T]] = Liftable[Value[T]] {
//    case ast: Value[SCollectionType[SType]] => collLiftable(ast)
//    case ast: Value[STuple]                 => tupleLiftable(ast)
//    case ast: Extract[T]             => extractLiftable(ast)
    case Outputs                     => q"$epack.Outputs"
    case Height                      => q"$epack.Height"
    case Self                        => q"$epack.Self"
    case IntConstant(v)              => q"$svpack.IntConstant($v)"
    case ast: SigmaTransformer[_, _] => sigmaTransLiftable(ast)
    case ast: Transformer[_, _]      => transLiftable(ast)
//    case ast: Relation[_, _]                    => relationLiftable(ast.asInstanceOf[Relation[SType, SType]])
    case BlockValue(stats, res) =>
      // TODO: WTF with list?
      q"$svpack.BlockValue(${stats.toList}.toIndexedSeq, $res)"
    case ast: Value[_] if ast.tpe == SSigmaProp => sigmaPropLiftable(ast.asSigmaProp)
//    case ast: NotReadyValue[_] if ast.tpe == SBoolean =>
//      notReadyBoolValueLiftable(ast.asInstanceOf[NotReadyValueBoolean])
//    case ast: Value[_] if ast.tpe == SBoolean => boolValueLiftable(ast.asBoolValue)
    // TODO: make it pretty
    case BinAnd(l, r) =>
      q"$spack.BinAnd($l.asInstanceOf[$svpack.Value[$spack.SBoolean.type]], $r.asInstanceOf[$svpack.Value[$spack.SBoolean.type]])"
    // TODO: cast to BoolValue as well?
    case BinOr(l, r)    => q"$spack.BinOr($l, $r)"
    case GT(l, r)       => q"$spack.GT($l, $r)"
    case GE(l, r)       => q"$spack.GE($l, $r)"
    case EQ(l, r)       => q"$spack.EQ($l, $r)"
    case ValUse(i, tpe) => q"$svpack.ValUse($i, $tpe)"
//    case OptionIsDefined(v) => q"$supack.OptionIdDefined($v)"

    case ast: ScalaTree => scalaTreeLiftable(ast)
//    case ast: Value[_]                        => svalueLiftable(ast)
//    case v @ _ => c.fail(s"no Liftable for SValue: $v")
  }

  implicit def stypeLiftable[T <: SType]: Liftable[T] = Liftable[T] {
    case SByte      => q"$spack.SByte"
    case SBoolean   => q"$spack.SBoolean"
    case SShort     => q"$spack.SShort"
    case SInt       => q"$spack.SInt"
    case SLong      => q"$spack.SLong"
    case SSigmaProp => q"$spack.SSigmaProp"
    case SCollectionType(eT) =>
      q"new $spack.SCollectionType($eT)" // otherwise scalac choose object with the same name
    case STuple(items) => q"$spack.STuple(..$items)"
    case SOption(eT)   => q"$spack.SOption($eT)"
    case v @ _         => c.fail(s"no Liftable for SType: $v")
  }

//  implicit def sOptionTypeLiftable[T <: SType]: Liftable[SOption[T]] =
//    Liftable[SOption[T]] {
////      case o: SOption[T] => q"$spack.SOption(${o.elemType})"
//      case SOption(SCollectionType(SByte)) => q"$spack.SOption(SCollectionType(SByte))"
//      case SOption(SCollectionType(STuple(Seq(SCollectionType(SByte), SLong)))) =>
//        q"$spack.SOption(SCollectionType(STuple(SCollectionType(SByte), SLong)))"
//      case v @ _ => c.fail(s"no Liftable for SOption: $v")
//    }

  implicit val sigmaTransLiftable: Liftable[SigmaTransformer[_, _]] =
    Liftable[SigmaTransformer[_, _]] {
      case SigmaOr(items) => q"$spack.SigmaOr(..$items)"
      case v @ _          => c.fail(s"no Liftable for SigmaTransformer: $v")
    }

//  implicit val boxLiftable = Liftable[Value[SBox.type]] {
//    case v @ _ => c.fail(s"no Liftable for Box: $v")
//  }

//  implicit def optionLiftable[T <: SType] = Liftable[Value[SOption[T]]] {
////    case ExtractRegisterAs(b, r, tpe) => q"$supack.ExtractRegisterAs($b, $r, $tpe)"
//    case v @ _ => c.fail(s"no Liftable for Option: $v")
//  }

//  implicit def collLiftable[T <: SType] = Liftable[Value[SCollection[T]]] {
//    case Outputs => q"$epack.Outputs"
//    case v @ _   => c.fail(s"no Liftable for Coll: $v")
//  }

  implicit def transLiftable[IV <: SType, OV <: SType]: Liftable[Transformer[IV, OV]] =
    Liftable[Transformer[IV, OV]] {
      case SizeOf(v)                      => q"$supack.SizeOf($v)"
      case ByIndex(c, i, d)               => q"$supack.ByIndex($c, $i, $d)"
      case OptionIsDefined(v)             => q"$supack.OptionIsDefined($v)"
      case SelectField(input, fieldIndex) => q"$supack.SelectField($input, $fieldIndex)"
      case ExtractRegisterAs(b, r, tpe)   => q"$supack.ExtractRegisterAs($b, $r, $tpe)"
      case ExtractBytes(b)                => q"$supack.ExtractBytes($b)"
      case ExtractId(b)                   => q"$supack.ExtractId($b)"
      case ExtractScriptBytes(b)          => q"$supack.ExtractScriptBytes($b)"
      case OptionGet(i)                   => q"$supack.OptionGet($i)"
      case SigmaPropBytes(i)              => q"$supack.SigmaPropBytes($i)"
      case v @ _                          => c.fail(s"no Liftable for Transformer: $v")
    }

//  implicit val tupleLiftable = Liftable[Value[STuple]] {
//    case v @ _ => c.fail(s"no Liftable for Tuple: $v")
//  }

//  implicit val notReadyBoolValueLiftable: Liftable[NotReadyValue[SBoolean.type]] =
//    Liftable[NotReadyValue[SBoolean.type]] {
//      case BinAnd(l, r) => q"$spack.BinAnd($l, $r)"
//      case BinOr(l, r)  => q"$spack.BinOr($l, $r)"
//      case v @ _        => c.fail(s"no Liftable for NotReadyValueBoolean: $v")
//    }

//  implicit val boolValueLiftable: Liftable[BoolValue] = Liftable[BoolValue] {
//    case BinAnd(l, r) => q"$spack.BinAnd($l, $r)"
//    case BinOr(l, r)  => q"$spack.BinOr($l, $r)"
//    case GT(l, r)     => q"$spack.GT($l, $r)"
//    case GE(l, r)     => q"$spack.GE($l, $r)"
////    case v @ _              => c.fail(s"no Liftable for BoolValue: $v")
//  }

  implicit val sigmaPropLiftable: Liftable[SigmaPropValue] = Liftable[SigmaPropValue] {
    case BoolToSigmaProp(v) => q"$spack.BoolToSigmaProp($v)"
    case v: ScalaTree       => scalaTreeLiftable(v)
    case v @ _              => c.fail(s"no Liftable for SigmaPropValue: $v")
  }

  implicit val scalaTreeLiftable: Liftable[ScalaTree] = Liftable[ScalaTree] {
    case st @ ScalaTree(t) => q"$svpack.Constant[${st.tpe}]($t, ${st.tpe})"
  }

//  implicit val relationLiftable: Liftable[Relation[SType, SType]] =
//    Liftable[Relation[SType, SType]] {
//      case v @ _ => c.fail(s"no Liftable for Relation: $v")
//    }

}
