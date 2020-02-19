package org.ergoplatform.compiler.compilation

import org.ergoplatform.Height
import org.ergoplatform.compiler.ErgoContract
import org.ergoplatform.compiler.compilation.util.RichContext
import sigmastate.SType
import special.sigma.{Context, SigmaProp}

import scala.collection.mutable
import scala.reflect.macros.{whitebox, TypecheckException}

trait Compilation extends Parsing with Liftables {
  val c: whitebox.Context
  import c.universe._

  def compileBody[A](body: Tree) = {
    valDefsMap = mutable.Map[String, (Int, SType)]()
    val prop = astParser(body)
    c.info(s"SValue: $prop")
    val tree =
//      c.untypecheck {
      q"$prop"
//      }
    c.info(s"Compiled tree: $tree")
    tree
  }

  private def findContractDefDef(select: Select): String = {
    import scala.meta._
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
      .collect { case sel: Select => sel }
      .headOption
      .getOrElse(c.fail("method call for the contract is expected"))
    val defdefSource = findContractDefDef(select)
    val ctxParamName = compilingClosure
      .collect { case ValDef(mods, termName, _, _) => termName }
      .headOption
      .getOrElse(c.fail("context parameter is expected"))
      .toString
    val compilingContractApp = compilingClosure
      .collect { case app: Apply => app }
      .headOption
      .getOrElse(c.fail("cannot find Apply for the contract method"))
    c.debug(compilingContractApp.args)
    val argsStr = compilingContractApp.args
      .flatMap(_.collect { case Ident(name) => name.toString })
      .filterNot(_ == "org")
      .mkString(",")
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
    val tree = c.parse(scalaFuncSource)
    try {
      c.typecheck(tree)
    } catch {
      case e: TypecheckException =>
        c.fail(
          s"Failed to typecheck with error: $e\n for source:\n $scalaFuncSource \n for tree: ${showRaw(tree)}"
        )
    }
  }

  def compileVerified[A, B](func: Expr[A => B]) = {
    c.info(s"compiling closure: ${showRaw(func.tree)}")
    val contractMethodName = func.tree
      .collect { case sel: Select => sel }
      .headOption
      .getOrElse(c.fail("method call for the contract is expected"))
      .name
      .toString
    val contractTree = buildScalaFunc(func.tree, addVerifiedTypeConv = true)
    val defDef = contractTree
      .collect { case dd @ DefDef(_, TermName(`contractMethodName`), _, _, _, _) => dd }
      .headOption
      .getOrElse(c.fail("cannot find DefDef for the contract method"))
    val compilingContractApp = func.tree
      .collect { case app: Apply => app }
      .headOption
      .getOrElse(c.fail("cannot find Apply for the contract method"))
    val appArgs = compilingContractApp.args.flatMap(_.collect {
      case Ident(name) => name.toString
    })

    val defDefArgNames = defDef.vparamss.head.collect {
      case ValDef(_, name, _, _) => name.toString
    }
    // TODO: use it
    val paramMap = defDefArgNames.zip(appArgs).toMap

    val sigmaProp = astParser(defDef.rhs)
    q"""
      ErgoContract(
        $contractTree,
        $sigmaProp
      )
     """
  }

  def compile(func: Expr[Context => SigmaProp]) =
    reify(
      ErgoContract(func.splice, reify(Height).splice)
    )

}
