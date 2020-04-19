package org.ergoplatform.compiler.compilation

import org.ergoplatform.compiler.compilation.util.RichContext
import sigmastate.SType
import sigmastate.Values.BlockValue
import special.sigma.{Context, SigmaProp}

import scala.collection.mutable
import scala.reflect.macros.{whitebox, TypecheckException}

//noinspection TypeAnnotation
trait Compilation extends Parsing with Liftables {
  val c: whitebox.Context
  import c.universe._

  def compileBody[A](body: Tree) = {
    valDefsMap        = mutable.Map[String, (Int, SType)]()
    callArgToIdentMap = Map()
    val prop = astParser(body)
    c.info(s"SValue: $prop")
    val tree =
//      c.untypecheck {
      q"""org.ergoplatform.compiler.ErgoContract(
            {_ => ??? },
            $prop
          )"""
//      }
    c.info(s"Compiled tree: $tree")
    tree
  }

  private def findContractDefDef(select: Select): String = {
    import scala.meta._
    // c.warn(s"getting file path for $select")
    val path   = select.symbol.pos.source.file.file.toPath
    val source = new String(java.nio.file.Files.readAllBytes(path), "UTF-8")
    val input  = Input.VirtualFile(path.toString, source)
    val tree   = input.parse[Source].get
    tree
      .collect {
        // TODO: check the full signature and not just the name
        case dd @ Defn.Def(mods, name, tparams, paramss, decltpe, body)
            if name.toString == select.name.toString =>
          dd.toString
      }
      .headOption
      .getOrElse(c.fail("cannot find DefDef for the contract method"))
  }

  private def buildScalaFunc(
    compilingClosure: Tree,
    addVerifiedTypeConv: Boolean
  ): Tree = {
    val select = compilingClosure
      .collect { case Apply(sel @ Select(_, _), _) => sel }
      .headOption
      .getOrElse(c.fail("method call for the contract is expected"))
    val defdefSource = findContractDefDef(select)
    val ctxParamName = compilingClosure
      .collect { case ValDef(_, termName, _, _) => termName }
      .headOption
      .getOrElse(c.fail("context parameter is expected"))
      .toString
    val compilingContractApp = compilingClosure
      .collect { case app: Apply => app }
      .headOption
      .getOrElse(c.fail("cannot find Apply for the contract method"))
    c.info(
      s"contract method Apply args: ${compilingContractApp.args} \n raw: ${compilingContractApp.args
        .map(showRaw(_))
        .mkString(",")}"
    )
    val argsStr = extractApplyArgNames(compilingContractApp).mkString(",")
    if (argsStr.isEmpty) c.fail("no arguments provided for the contract call")
    val scalaFuncSource =
      s"""
         |{ $ctxParamName: special.sigma.Context =>
         |import special.sigma._
         |import special.collection._
         |import org.ergoplatform.dsl._
         |${if (addVerifiedTypeConv)
           "import org.ergoplatform.sigma.verified.VerifiedTypeConverters._"
         else ""}
         |
         |object SigmaContractHolder extends SigmaContractSyntax {
         |  import syntax._
         |  lazy val spec = ???
         |  lazy val contractEnv = ???
         |
         |  //implicit def booleanToSigmaProp(source: Boolean): SigmaProp = this.builder.sigmaProp(source)
         |
         |  $defdefSource
         |}
         |
         |SigmaContractHolder.${select.name.toString}($argsStr)
         |}
         |""".stripMargin
    c.info(s"parsing generated source: $scalaFuncSource")
    val tree = c.parse(scalaFuncSource)
    c.info(s"parsed contract method tree: ${showRaw(tree)}")
    try {
      c.typecheck(tree)
    } catch {
      case e: TypecheckException =>
        c.fail(
          s"Failed to typecheck with error: $e\n for source:\n $scalaFuncSource \n for tree: ${showRaw(tree)}"
        )
    }
  }

  private def extractApplyArgNames(compilingContractApp: c.universe.Apply): Seq[String] =
    compilingContractApp.args
      .flatMap(_.collect { case Ident(TermName(name)) => name })
      .filterNot(_ == "org")

  private def compileExternalMethodCall[A, B](
    func: Expr[A => B],
    addVerifiedTypeConv: Boolean
  ) = {
    c.info(s"compiling closure: ${showRaw(func.tree)}")
    val contractMethodName = func.tree
      .collect { case sel: Select => sel }
      .headOption
      .getOrElse(c.fail("method call for the contract is expected"))
      .name
      .toString
    val assembledContractBodyScalaTree =
      buildScalaFunc(func.tree, addVerifiedTypeConv = addVerifiedTypeConv)
    val defDef = assembledContractBodyScalaTree
      .collect { case dd @ DefDef(_, TermName(`contractMethodName`), _, _, _, _) => dd }
      .headOption
      .getOrElse(c.fail("cannot find DefDef for the contract method"))
    val compilingContractApply = func.tree
      .collect { case app: Apply => app }
      .headOption
      .getOrElse(c.fail("cannot find Apply for the contract method"))
    val appArgs = extractApplyArgNames(compilingContractApply)

    val defDefArgNames = defDef.vparamss.head.collect {
      case ValDef(_, name, _, _) => name.toString
    }
    val innerBodyArgNamesToApplyArgNames = defDefArgNames.zip(appArgs).toMap
    c.info(s"paramMAp: $innerBodyArgNamesToApplyArgNames")

    callArgToIdentMap = innerBodyArgNamesToApplyArgNames
    val sigmaProp = astParser(defDef.rhs)
    c.info(s"compiled ergo tree: $sigmaProp")
    val unwrappedBlockValue = sigmaProp match {
      case BlockValue(IndexedSeq(), body) => body
      case v                              => v
    }
    c.info(s"unwrapped BlockValue: $unwrappedBlockValue")
    // c.untypecheck {
    q"""
      ErgoContract(
        $assembledContractBodyScalaTree,
        $unwrappedBlockValue
      )
     """
    // }
  }

  def compileVerified[A, B](func: Expr[A => B]) =
    compileExternalMethodCall(func, addVerifiedTypeConv = true)

  def compile(func: Expr[Context => SigmaProp]) =
    compileExternalMethodCall(func, addVerifiedTypeConv = false)
}
