package org.ergoplatform.compiler

import sigmastate.Values.{ErgoTree, SValue}
import sigmastate.lang.Terms.ValueOps
import special.sigma.{Context, SigmaProp}

case class ErgoContract(scalaFunc: Context => SigmaProp, prop: SValue) {
  val ergoTree: ErgoTree = ErgoTree.fromProposition(prop.asSigmaProp)
}
