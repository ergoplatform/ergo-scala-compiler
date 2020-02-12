package org.ergoplatform.compiler

import special.sigma.{Context, SigmaProp}

import scala.language.experimental.macros

object ErgoScalaCompiler {

  // TODO add scaladoc

  /** Compiles Context => SigmaProp
    * @param contract
    * @tparam A
    * @tparam B
    * @return
    */
  def compileVerified[A, B](contract: A => B): ErgoContract =
    macro ErgoScalaVerifiedCompiler.compile[A, B]

  def compile(contract: Context => SigmaProp): ErgoContract =
    macro ErgoScalaNonVerifiedCompiler.compile
}
