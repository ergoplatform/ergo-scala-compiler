package org.ergoplatform.compiler.test.contracts.dex.verified

import org.ergoplatform.compiler.{ErgoContract, ErgoScalaCompiler}
import special.collection.Coll
import special.sigma.SigmaProp

object AssetsAtomicExchangeVerifiedCompilationConverted
  extends AssetsAtomicExchangeVerified {

  def buyerContractInstanceNonVerifiedTypes(
    tokenId: Coll[Byte],
    tokenAmount: Long,
    pkA: SigmaProp
  ): ErgoContract = {
    import org.ergoplatform.sigma.verified.VerifiedTypeConverters._
    ErgoScalaCompiler.contractVerified {
      context: org.ergoplatform.sigma.verified.Context =>
        buyer(context, tokenId, tokenAmount, pkA)
    }
  }

  def sellerContractInstanceNonVerifiedTypes(
    ergAmount: Long,
    pkB: SigmaProp
  ): ErgoContract = {
    import org.ergoplatform.sigma.verified.VerifiedTypeConverters._
    ErgoScalaCompiler.contractVerified {
      context: org.ergoplatform.sigma.verified.Context =>
        seller(context, ergAmount, pkB)
    }
  }

}
