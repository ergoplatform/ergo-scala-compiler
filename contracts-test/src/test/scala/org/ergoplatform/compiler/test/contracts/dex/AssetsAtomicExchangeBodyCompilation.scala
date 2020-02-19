package org.ergoplatform.compiler.test.contracts.dex

import org.ergoplatform.compiler.ErgoScalaCompiler._
import special.collection.Coll
import special.sigma.SigmaProp
import org.ergoplatform.compiler.test.ObjectGenerators._

object AssetsAtomicExchangeBodyCompilation {

  val tokenId: Coll[Byte] = newTokenId

  val buyerBidTokenAmount = 100L
  val buyer: SigmaProp    = newPK
  val seller: SigmaProp   = newPK
  val sellerAskNanoErgs   = 50000000

  val capturedInt  = 999
  val capturedInt2 = 999

//  val dummyContract = contract {
//    capturedInt + capturedInt2
//  }

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

//  val sellerContract = contract {
//    seller || (
//      OUTPUTS.size > 1 &&
//      OUTPUTS(1).R4[Coll[Byte]].isDefined
//    ) && {
//      val knownBoxId = OUTPUTS(1).R4[Coll[Byte]].get == SELF.id
//      OUTPUTS(1).value >= sellerAskNanoErgs &&
//      knownBoxId &&
//      OUTPUTS(1).propositionBytes == seller.propBytes
//    }
//  }
}
