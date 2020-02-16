package org.ergoplatform.compiler.test.contracts.dex.verified

import org.ergoplatform.compiler._
import org.ergoplatform.sigma.verified._

import scala.language.{implicitConversions, postfixOps}

sealed abstract class AssetsAtomicExchangeVerified extends SigmaContract {

  def buyer(
    ctx: Context,
    tokenId: Coll[Byte],
    tokenAmount: Long,
    pkA: SigmaProp
  ): SigmaProp = {
    import ctx._
    pkA || {
      (OUTPUTS.nonEmpty && OUTPUTS(0).R4[Coll[Byte]].isDefined) && {
        val tokens = OUTPUTS(0).tokens
        val tokenDataCorrect = tokens.nonEmpty &&
          tokens(0)._1 == tokenId &&
          tokens(0)._2 >= tokenAmount

        val knownId = OUTPUTS(0).R4[Coll[Byte]].get == SELF.id
        // TODO fix Coll.fromItems crashing Inox typer and rewrite with allOf(Coll.fromItems[Boolean](
        tokenDataCorrect &&
        OUTPUTS(0).propositionBytes == pkA.propBytes &&
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

object AssetsAtomicExchangeVerifiedCompilation extends AssetsAtomicExchangeVerified {

  def buyerContractInstance(
    tokenId: Coll[Byte],
    tokenAmount: Long,
    pkA: SigmaProp
  ): ErgoContract =
    ErgoScalaCompiler.contractVerified { context: Context =>
      buyer(context, tokenId, tokenAmount, pkA)
    }

  def sellerContractInstance(ergAmount: Long, pkB: SigmaProp): ErgoContract =
    ErgoScalaCompiler.contractVerified { context: Context =>
      seller(context, ergAmount, pkB)
    }

}
