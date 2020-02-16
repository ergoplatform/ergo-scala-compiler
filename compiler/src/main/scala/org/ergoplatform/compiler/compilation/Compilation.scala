package org.ergoplatform.compiler.compilation

import org.ergoplatform.Height
import org.ergoplatform.compiler.ErgoContract
import special.sigma.{Context, SigmaProp}

import scala.reflect.macros.whitebox

trait Compilation {
  val c: whitebox.Context
  import c.universe._

  def compileBody[A](body: Tree) = reify(Height)

  def compileVerified[A, B](func: Tree) =
    reify(
      ErgoContract(c.Expr[Context => SigmaProp](q"""""").splice, reify(Height).splice)
    )

  def compile(func: Expr[Context => SigmaProp]) =
    reify(
      ErgoContract(func.splice, reify(Height).splice)
    )

}
