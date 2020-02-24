package org.ergoplatform.compiler.test

import org.ergoplatform.ErgoBox.TokenId
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval.CSigmaProp
import sigmastate.eval.Extensions.ArrayOps
import sigmastate.interpreter.CryptoConstants
import special.collection.Coll
import special.sigma.SigmaProp

object ObjectGenerators {

  def newTokenId: Coll[Byte] =
    Array.fill(TokenId.size)((scala.util.Random.nextInt(256) - 128).toByte).toColl

  def newPK: SigmaProp =
    CSigmaProp(ProveDlog(CryptoConstants.dlogGroup.createRandomElement()))

}
