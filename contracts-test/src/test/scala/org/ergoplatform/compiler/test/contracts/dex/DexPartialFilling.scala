package org.ergoplatform.compiler.test.contracts.dex

import org.ergoplatform.compiler.{ErgoContract, ErgoScalaCompiler}
import special.collection.Coll
import special.sigma.{Context, SigmaContract, SigmaDslBuilder, SigmaProp}

sealed abstract class DexPartialFilling extends SigmaContract {

  override def builder: SigmaDslBuilder = ???

  def buyer(
    ctx: Context,
    buyerPk: SigmaProp,
    tokenId: Coll[Byte],
    tokenPrice: Long,
    dexFeePerToken: Long
  ): SigmaProp = {
    import ctx._
    buyerPk || {

      val returnBox = OUTPUTS(0)
      // val returnBox = OUTPUTS.filter { b =>
      //   b.R4[Coll[Byte]].isDefined && b
      //     .R4[Coll[Byte]]
      //     .get == SELF.id && b.propositionBytes == buyerPk.propBytes
      // }(0)

      val returnTokenData        = returnBox.tokens(0)
      val returnTokenId          = returnTokenData._1
      val returnTokenAmount      = returnTokenData._2
      val maxReturnTokenErgValue = returnTokenAmount * tokenPrice
      val totalReturnErgValue    = maxReturnTokenErgValue + returnBox.value
      val expectedDexFee         = dexFeePerToken * returnTokenAmount

      val foundNewOrderBoxes = OUTPUTS
      // val foundNewOrderBoxes = OUTPUTS.filter { b =>
      //   b.R4[Coll[Byte]].isDefined && b
      //     .R4[Coll[Byte]]
      //     .get == SELF.id && b.propositionBytes == SELF.propositionBytes
      // }

      val coinsSecured = (SELF.value - expectedDexFee) == maxReturnTokenErgValue || {
        foundNewOrderBoxes.size == 1 && foundNewOrderBoxes(0).value >= (SELF.value - totalReturnErgValue - expectedDexFee)
      }

      val tokenIdIsCorrect = returnTokenId == tokenId

      tokenIdIsCorrect && returnTokenAmount >= 1 && coinsSecured
    }
  }

  // def seller(ctx: Context, ergAmount: Long, pkB: SigmaProp): SigmaProp = {
  //   import ctx._
  //   pkB || (
  //     OUTPUTS.size > 1 &&
  //     OUTPUTS(1).R4[Coll[Byte]].isDefined
  //   ) && {
  //     val knownBoxId = OUTPUTS(1).R4[Coll[Byte]].get == SELF.id
  //     OUTPUTS(1).value >= ergAmount &&
  //     knownBoxId &&
  //     OUTPUTS(1).propositionBytes == pkB.propBytes
  //   }
  // }
}

object DexPartialFillingCompilation extends DexPartialFilling {

  def buyerContractInstance(
    buyerPk: SigmaProp,
    tokenId: Coll[Byte],
    tokenPrice: Long,
    dexFeePerToken: Long
  ): ErgoContract =
    ErgoScalaCompiler.contract { context: Context =>
      buyer(context, buyerPk, tokenId, tokenPrice, dexFeePerToken)
    }

  // def sellerContractInstance(ergAmount: Long, pkB: SigmaProp): ErgoContract =
  //   ErgoScalaCompiler.contract { context: Context =>
  //     seller(context, ergAmount, pkB)
  //   }

}
