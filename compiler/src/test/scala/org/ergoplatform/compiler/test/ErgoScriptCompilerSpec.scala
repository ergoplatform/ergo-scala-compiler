package org.ergoplatform.compiler.test

import sigmastate.Values._
import sigmastate.eval.CSigmaProp
import sigmastate.helpers.{SigmaTestingCommons}
import sigmastate.serialization.generators.ObjectGenerators

import org.ergoplatform.compiler.ErgoScalaCompiler

class ErgoScriptCompilerSpec extends SigmaTestingCommons with ObjectGenerators {

  implicit lazy val IR: TestingIRContext = new TestingIRContext

  property("smoke test") {
    forAll(proveDlogGen) { dLog =>
      val env = Map("buyerPk" -> CSigmaProp(dLog))
      val c   = ErgoScalaCompiler.contract(env, "buyerPk")
      c.ergoTree.root.right.get shouldEqual SigmaPropConstant(dLog),
    }
  }
}
