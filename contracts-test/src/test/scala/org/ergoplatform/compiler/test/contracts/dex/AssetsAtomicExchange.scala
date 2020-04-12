package org.ergoplatform.compiler.test.contracts.dex

import org.ergoplatform.compiler.{ErgoContract, ErgoScalaCompiler}
import special.collection.Coll
import special.sigma.{Context, SigmaContract, SigmaDslBuilder, SigmaProp}

sealed abstract class AssetsAtomicExchange extends SigmaContract {

  override def builder: SigmaDslBuilder = ???

  def buyer(
    ctx: Context,
    tokenId: Coll[Byte],
    tokenAmount: Long,
    buyerPk: SigmaProp
  ): SigmaProp = {
    import ctx._
    buyerPk || {
      (OUTPUTS.nonEmpty && OUTPUTS(0).R4[Coll[Byte]].isDefined) && {
        val tokens = OUTPUTS(0).tokens
        val tokenDataCorrect = tokens.nonEmpty &&
          tokens(0)._1 == tokenId &&
          tokens(0)._2 >= tokenAmount

        val knownId = OUTPUTS(0).R4[Coll[Byte]].get == SELF.id
        // TODO fix Coll.fromItems crashing Inox typer and rewrite with allOf(Coll.fromItems[Boolean](
        tokenDataCorrect &&
        OUTPUTS(0).propositionBytes == buyerPk.propBytes &&
        knownId
      }
    }
  }

  def seller(ctx: Context, ergAmount: Long, pkB: SigmaProp): SigmaProp = {
    import ctx._
    pkB || (
      OUTPUTS.size > 1 &&
      OUTPUTS(1).R4[Coll[Byte]].isDefined
    ) && {
      val knownBoxId = OUTPUTS(1).R4[Coll[Byte]].get == SELF.id
      OUTPUTS(1).value >= ergAmount &&
      knownBoxId &&
      OUTPUTS(1).propositionBytes == pkB.propBytes
    }
  }
}

object AssetsAtomicExchangeCompilation extends AssetsAtomicExchange {

  def buyerContractInstance(
    tokenId: Coll[Byte],
    tokenAmount: Long,
    pkA: SigmaProp
  ): ErgoContract =
    ErgoScalaCompiler.contract { context: Context =>
      buyer(context, tokenId, tokenAmount, pkA)
    }

  def sellerContractInstance(ergAmount: Long, pkB: SigmaProp): ErgoContract =
    ErgoScalaCompiler.contract { context: Context =>
      seller(context, ergAmount, pkB)
    }

}
