package org.ergoplatform.compiler.test.contracts.dex

import org.ergoplatform.compiler.ErgoScalaCompiler.contract
import org.ergoplatform.compiler.test.BodySyntax.{Coll, OUTPUTS, SELF, SigmaProp}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval.CSigmaProp

object AssetsAtomicExchangeBodyCompilation {

  val tokenId: Coll[Byte] = ???

  val buyerBidTokenAmount        = 100L
  val buyerProveDlog: ProveDlog  = ???
  val buyer: SigmaProp           = CSigmaProp(buyerProveDlog)
  val sellerProveDlog: ProveDlog = ???
  val seller: SigmaProp          = CSigmaProp(sellerProveDlog)
  val sellerAskNanoErgs          = 50000000

  val buyerContract = contract {
    buyer || {
      (OUTPUTS.nonEmpty && OUTPUTS(0).R4[Coll[Byte]].isDefined) && {
        val tokens = OUTPUTS(0).tokens
        val tokenDataCorrect = tokens.nonEmpty &&
          tokens(0)._1 == tokenId &&
          tokens(0)._2 >= buyerBidTokenAmount

        val knownId = OUTPUTS(0).R4[Coll[Byte]].get == SELF.id
        tokenDataCorrect && OUTPUTS(0).propositionBytes == buyer.propBytes && knownId
      }
    }
  }

  val sellerContract = contract {
    seller || (
      OUTPUTS.size > 1 &&
      OUTPUTS(1).R4[Coll[Byte]].isDefined
    ) && {
      val knownBoxId = OUTPUTS(1).R4[Coll[Byte]].get == SELF.id
      OUTPUTS(1).value >= sellerAskNanoErgs &&
      knownBoxId &&
      OUTPUTS(1).propositionBytes == seller.propBytes
    }
  }
}
