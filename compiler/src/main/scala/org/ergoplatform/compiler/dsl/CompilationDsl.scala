package org.ergoplatform.compiler.dsl

import org.ergoplatform.compiler.ErgoContract
import org.ergoplatform.compiler.compilation.Compilation
import sigmastate.Values.{ErgoTree, SValue, SigmaPropValue}
import special.sigma.{Context, SigmaProp}

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.whitebox

trait CompilationDsl {

  def contract[A](body: A): ErgoContract = macro CompilationMacro.compileBody[A]

  def contractVerified[A, B](func: A => B): ErgoContract =
    macro CompilationMacro.compileVerified[A, B]

  def contract(func: Context => SigmaProp): ErgoContract =
    macro CompilationMacro.compile

}

class CompilationMacro(val c: whitebox.Context) extends Compilation
