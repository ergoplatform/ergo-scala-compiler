package org.ergoplatform.compiler.test.contracts.dex

import org.ergoplatform.compiler.ErgoContract
import org.ergoplatform.compiler.ErgoScalaCompiler._
import special.collection.Coll
import special.sigma.SigmaProp

object AssetsAtomicExchangeBodyCompilation {

  def buyerContract(
    tokenId: Coll[Byte],
    tokenAmount: Long,
    buyerPk: SigmaProp
  ): ErgoContract =
    contract {
      buyerPk || {
        (OUTPUTS.nonEmpty && OUTPUTS(0).R4[Coll[Byte]].isDefined) && {
          val tokens = OUTPUTS(0).tokens
          val tokenDataCorrect = tokens.nonEmpty &&
            tokens(0)._1 == tokenId &&
            tokens(0)._2 >= tokenAmount

          val knownId = OUTPUTS(0).R4[Coll[Byte]].get == SELF.id
          tokenDataCorrect && OUTPUTS(0).propositionBytes == buyerPk.propBytes && knownId
        }
      }
    }

  def sellerContract(askNanoErgs: Long, sellerPk: SigmaProp): ErgoContract =
    contract {
      sellerPk || (
        OUTPUTS.size > 1 &&
        OUTPUTS(1).R4[Coll[Byte]].isDefined
      ) && {
        val knownBoxId = OUTPUTS(1).R4[Coll[Byte]].get == SELF.id
        OUTPUTS(1).value >= askNanoErgs &&
        knownBoxId &&
        OUTPUTS(1).propositionBytes == sellerPk.propBytes
      }
    }
}
