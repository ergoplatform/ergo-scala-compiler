package org.ergoplatform.compiler.compilation

import org.ergoplatform.ErgoBox.{R2, R4}
import org.ergoplatform.{ErgoBox, Height, Outputs, Self}

import scala.reflect.macros.whitebox.{Context => MacroContext}
import scala.reflect.ClassTag
import org.ergoplatform.compiler.compilation.util.RichContext
import sigmastate.{
  BinAnd,
  BinOr,
  BoolToSigmaProp,
  EQ,
  GE,
  GT,
  LE,
  LT,
  Relation,
  SByte,
  SCollection,
  SOption,
  SType,
  SigmaAnd,
  SigmaOr,
  SigmaTransformer,
  Values
}
import sigmastate.lang.Terms.ValueOps
import sigmastate.Values.{
  BlockValue,
  GroupGenerator,
  IntConstant,
  IntValue,
  NotReadyValue,
  SValue,
  ValUse,
  Value
}
import sigmastate.utxo.{
  ByIndex,
  ExtractAmount,
  ExtractId,
  ExtractRegisterAs,
  ExtractScriptBytes,
  OptionGet,
  OptionIsDefined,
  SelectField,
  SigmaPropBytes,
  SizeOf
}
import special.sigma.SigmaContract

import scala.collection.mutable
import scala.reflect.api.Trees
import sigmastate.serialization.OpCodes
import sigmastate.ArithOp
import sigmastate.Values.LongConstant

trait Parsing {
  this: Compilation =>

  val c: MacroContext
  import c.universe._
  import c.universe.definitions._

  private def is[T](tree: Tree)(implicit t: TypeTag[T]) =
    tree.tpe <:< t.tpe

  case class Parser[T](p: PartialFunction[Trees#Tree, T])(implicit ct: ClassTag[T]) {

    def apply(tree: Tree): T =
      unapply(tree).getOrElse {
        c.fail(
          s"Tree: '$tree'\nRaw: ${c.universe.showRaw(tree)}\nSymbol: ${tree.symbol}, tpe: ${tree.tpe}) can't be parsed to '${ct.runtimeClass.getSimpleName}'"
        )
      }

    def unapply(tree: Tree): Option[T] =
      tree match {
        case other =>
          p.lift(tree)
      }
  }

  val astParser: Parser[SValue] = Parser[SValue] {
    case q"$i: $typ"             => astParser(i)
    case `constParser`(v)        => v
    case `contextApiParser`(v)   => v
    case `contractApiParser`(v)  => v
    case `sigmaTransParser`(v)   => v
    case `relationParser`(v)     => v
    case `twoArgOpParser`(v)     => v
    case `collApiParser`(v)      => v
    case `boxApiParser`(v)       => v
    case `sigmaPropApiParser`(v) => v
    case `optionApiParser`(v)    => v
    case `tupleParser`(v)        => v
    case `blockValueParser`(v)   => v
    case `identParser`(v)        => v

    // "catch them all" type of case, needs to be last
    case `capturedValParser`(v) => v
//    case t: Tree                => ScalaTree(t)
  }

  // TODO: remove?
  var callArgToIdentMap: Map[String, String]        = Map[String, String]()
  var valDefsMap: mutable.Map[String, (Int, SType)] = mutable.Map[String, (Int, SType)]()

  val identParser: Parser[NotReadyValue[SType]] = Parser[NotReadyValue[SType]] {
//    case t: ValDef => identClean(NamedValDef(t.name.decodedName.toString, astParser(t.rhs)))
    case i @ Ident(TermName(name)) if valDefsMap.get(cname(name)).nonEmpty =>
      val cleanName = cname(name)
      val (id, tpe) =
        valDefsMap.getOrElse(cleanName, c.fail(s"cannot find id for Ident($cleanName)"))
      ValUse(id, tpe)
  }

  private def cname(x: String): String = x.replace("$", "")

  val blockValueParser: Parser[BlockValue] = Parser[BlockValue] {
    case q"{..$exprs}" if exprs.size > 1 =>
      val stmts = exprs.take(exprs.length - 1)
      // TODO: not needed with valDefsMap
      val (lastVdExprs, lastId, lastVdIds) = stmts
        .filter(_.isInstanceOf[ValDefApi])
        .foldLeft((List.empty[Values.ValDef], 0, Map[String, (Int, SType)]())) {
          case ((valDefsExpr, lastUsedId, vdIds), ValDef(_, TermName(n), tpt, rhs)) =>
            val curId = lastUsedId + 1
            valDefsMap.put(n, (curId, tpeToSType(rhs.tpe)))
            (
              valDefsExpr :+ Values.ValDef(curId, astParser(rhs)),
              curId,
              vdIds + (n -> ((curId, tpeToSType(rhs.tpe))))
            )
        }

      val expr = astParser(exprs.last)
      BlockValue(lastVdExprs.toIndexedSeq, expr)
  }

  val capturedValParser: Parser[ScalaTree] = Parser[ScalaTree] {
//    case t: Select if is[SigmaContractDsl](t.qualifier) => c.fail(s"add parsing of: $t")
//    case t: Select if is[SigmaContextDsl](t.qualifier)  => c.fail(s"add parsing of: $t")
    case i @ Ident(TermName(name)) =>
      val newName =
        if (callArgToIdentMap.get(cname(name)).nonEmpty) callArgToIdentMap(cname(name))
        else name
      val v = Ident(TermName(newName))
      c.info(s"Capturing converted($name -> $newName}): ${showRaw(v)}")
      ScalaTree(v, tpeToSType(i.tpe))
    case t: Tree =>
      c.fail(s"Capturing non-Ident node: ${showRaw(t)}")
    // ScalaTree(t, tpeToSType(t.tpe))
  }

  val sigmaTransParser: Parser[SigmaTransformer[_, _]] = Parser[SigmaTransformer[_, _]] {

    case Apply(Select(lhs, m), Seq(arg)) if isTypeSigmaProp(lhs.tpe.widen) =>
      val l = astParser(lhs)
      val r = astParser(arg)
      m match {
        case TermName("$amp$amp") =>
          arg.tpe.widen match {
            case BooleanTpe => SigmaAnd(l.asSigmaProp, BoolToSigmaProp(r.asBoolValue))
            case _          => SigmaAnd(l.asSigmaProp, r.asSigmaProp)
          }
        case TermName("$bar$bar") =>
          arg.tpe.widen match {
            case BooleanTpe => SigmaOr(l.asSigmaProp, BoolToSigmaProp(r.asBoolValue))
            case _          => SigmaOr(l.asSigmaProp, r.asSigmaProp)
          }
      }
  }

  val relationParser: Parser[Relation[_, _]] = Parser[Relation[_, _]] {
    case Apply(Select(lhs, m), Seq(arg))
        if isTypeBoolean(lhs.tpe) || isTypeBoolean(arg.tpe) =>
      val l = astParser(lhs)
      val r = astParser(arg)
      m match {
        case TermName("$amp$amp") =>
          BinAnd(l.asBoolValue, r.asBoolValue)
        case TermName("$bar$bar") =>
          BinOr(l.asBoolValue, r.asBoolValue)
        case _ =>
          c.fail(
            s"relationParser: object $lhs(tpe: ${lhs.tpe.widen}) has unexpected $m with arg: $arg"
          )
      }
    case Apply(Select(astParser(l), TermName("$eq$eq")), Seq(astParser(r)))   => EQ(l, r)
    case Apply(Select(astParser(l), TermName("$less$eq")), Seq(astParser(r))) => LE(l, r)
    case Apply(Select(astParser(l), TermName("$greater$eq")), Seq(astParser(r))) =>
      GE(l, r)
    case Apply(Select(astParser(l), TermName("$greater")), Seq(astParser(r))) => GT(l, r)
    case Apply(Select(astParser(l), TermName("$less")), Seq(astParser(r)))    => LT(l, r)
  }

  val twoArgOpParser: Parser[SValue] = Parser[SValue] {
    case Apply(Select(astParser(l), TermName("$times")), Seq(astParser(r))) =>
      ArithOp(l, r, OpCodes.MultiplyCode)
    case Apply(Select(astParser(l), TermName("$plus")), Seq(astParser(r))) =>
      ArithOp(l, r, OpCodes.PlusCode)
    case Apply(Select(astParser(l), TermName("$minus")), Seq(astParser(r))) =>
      ArithOp(l, r, OpCodes.MinusCode)
  }

  val tupleParser: Parser[SValue] = Parser[SValue] {
    case q"$s._1" if isTypeTuple(s.tpe) => SelectField(astParser(s).asTuple, 1)
    case q"$s._2" if isTypeTuple(s.tpe) => SelectField(astParser(s).asTuple, 2)
  }

  val constParser: Parser[SValue] = Parser[SValue] {
    case Literal(ct @ c.universe.Constant(i)) if ct.tpe == IntTpe =>
      IntConstant(i.asInstanceOf[Int])
    case Literal(ct @ c.universe.Constant(i)) if ct.tpe == LongTpe =>
      LongConstant(i.asInstanceOf[Long])
  }

  val intValueParser: Parser[IntValue] = Parser[IntValue] {
    case Literal(ct @ c.universe.Constant(i)) if ct.tpe == IntTpe =>
      IntConstant(i.asInstanceOf[Int])
  }

  val collValueParser: Parser[Value[SCollection[SType]]] =
    Parser[Value[SCollection[SType]]] {
      case q"$s" if isTypeColl(s.tpe) => astParser(s).asCollection[SType]
    }

  val collApiParser: Parser[SValue] = Parser[SValue] {
    case q"${collValueParser(c)}.nonEmpty"                    => GT(SizeOf(c), IntConstant(0))
    case q"${collValueParser(c)}.size"                        => SizeOf(c)
    case q"${collValueParser(c)}.apply(${intValueParser(i)})" => ByIndex(c, i)
  }

  val optionApiParser: Parser[SValue] = Parser[SValue] {
    case q"$s.isDefined" if isTypeOption(s.tpe) =>
      OptionIsDefined(astParser(s).asOption[SType])
    case q"$s.get" if isTypeOption(s.tpe) =>
      OptionGet(astParser(s).asOption[SType])
  }

  val boxApiParser: Parser[SValue] = Parser[SValue] {
    case q"$s.id" if isTypeBox(s.tpe) =>
      ExtractId(astParser(s).asBox)
    case q"$s.R4[..$tpts](...$rType)" if isTypeBox(s.tpe) =>
      // TODO parse type
      ExtractRegisterAs(astParser(s).asBox, R4, SOption(SCollection(SByte)))
    case q"$s.tokens" if isTypeBox(s.tpe) =>
      OptionGet(
        ExtractRegisterAs(astParser(s).asBox, R2, SOption(ErgoBox.STokensRegType))
      )
    case q"$s.propositionBytes" if isTypeBox(s.tpe) =>
      ExtractScriptBytes(astParser(s).asBox)
    case q"$s.value" if isTypeBox(s.tpe) =>
      ExtractAmount(astParser(s).asBox)
  }

  val sigmaPropApiParser: Parser[SValue] = Parser[SValue] {
    case q"$s.propBytes" if isTypeSigmaProp(s.tpe) =>
      SigmaPropBytes(astParser(s).asSigmaProp)
  }

  val contextApiParser: Parser[SValue] = Parser[SValue] {
    case q"$s.OUTPUTS" if is[special.sigma.Context](s) => Outputs
    case q"$s.SELF" if is[special.sigma.Context](s)    => Self
  }

  val contractApiParser: Parser[SValue] = Parser[SValue] {
    case q"$s.HEIGHT" if is[SigmaContract](s)         => Height
    case q"$s.groupGenerator" if is[SigmaContract](s) => GroupGenerator
  }

}
