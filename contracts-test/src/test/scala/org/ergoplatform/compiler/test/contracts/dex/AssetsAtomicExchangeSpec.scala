package org.ergoplatform.compiler.test.contracts.dex

import org.ergoplatform.ErgoBox.{R2, R4}
import org.ergoplatform._
import org.ergoplatform.compiler.ErgoContract
import org.ergoplatform.compiler.test.contracts.dex.verified.AssetsAtomicExchangeVerifiedCompilationConverted
import org.scalacheck.Arbitrary.arbLong
import scorex.crypto.hash.Digest32
import sigmastate.Values._
import sigmastate._
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval.CSigmaProp
import sigmastate.eval.Extensions._
import sigmastate.helpers.{
  ContextEnrichingTestProvingInterpreter,
  ErgoLikeContextTesting,
  ErgoLikeTestInterpreter,
  SigmaTestingCommons
}
import sigmastate.interpreter.ProverResult
import sigmastate.serialization.generators.ObjectGenerators
import sigmastate.utxo._
import special.collection.Coll
import special.sigma.SigmaProp

import scala.language.implicitConversions

class AssetsAtomicExchangeSpec extends SigmaTestingCommons with ObjectGenerators {

  type BuyerContractSource  = (Coll[Byte], Long, SigmaProp) => ErgoContract
  type SellerContractSource = (Long, SigmaProp) => ErgoContract

  implicit lazy val IR: TestingIRContext = new TestingIRContext

  private def ctx(
    height: Int,
    tx: ErgoLikeTransaction,
    selfBox: ErgoBox = fakeSelf
  ): ErgoLikeContext =
    ErgoLikeContextTesting(
      currentHeight       = height,
      lastBlockUtxoRoot   = AvlTreeData.dummy,
      minerPubkey         = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend        = IndexedSeq(selfBox),
      spendingTransaction = tx,
      self                = selfBox
    )

  implicit private def toSigmaContext(ergoCtx: ErgoLikeContext): special.sigma.Context =
    ergoCtx.toSigmaContext(IR, false)

  private def testBuyerContractCancels(
    contractSource: BuyerContractSource,
    testScalaFunc: Boolean = true
  ) = {
    val prover      = new ContextEnrichingTestProvingInterpreter
    val verifier    = new ErgoLikeTestInterpreter
    val tokenId     = tokenIdGen.sample.get
    val tokenAmount = 100L
    val pubkey      = prover.dlogSecrets.head.publicImage

    val contract = contractSource(
      tokenId.asInstanceOf[Array[Byte]].toColl,
      tokenAmount,
      CSigmaProp(pubkey)
    )
    val tree = contract.ergoTree

    val spendingTransaction = createTransaction(
      IndexedSeq(
        ErgoBox(1, pubkey, 0, Seq((tokenIdGen.sample.get, 0))) // non-empty tokens as a workaround for
        // https://github.com/ScorexFoundation/sigmastate-interpreter/issues/628
      )
    )
    val context = ctx(50, spendingTransaction)

    val pr = prover.prove(tree, context, fakeMessage).get
    verifier.verify(tree, context, pr, fakeMessage).get._1 shouldBe true

    if (testScalaFunc) contract.scalaFunc(context) shouldEqual CSigmaProp(pubkey)
  }

  private def testBuyerContractNoTokens(
    contractSource: BuyerContractSource,
    testScalaFunc: Boolean = true
  ) = {
    val verifier           = new ErgoLikeTestInterpreter
    val prover             = new ContextEnrichingTestProvingInterpreter
    val pubkey             = prover.dlogSecrets.head.publicImage
    val buyerPk: SigmaProp = CSigmaProp(pubkey)
    val txNoTokens         = createTransaction(IndexedSeq(ErgoBox(1, TrivialProp.TrueProp, 0)))

    forAll(tokenIdGen.map(_.toColl), arbLong.arbitrary) {
      case (tokenId, tokenAmount) =>
        val contract = contractSource(tokenId, tokenAmount, buyerPk)

        val context = ctx(50, txNoTokens)
        verifier
          .verify(
            contract.ergoTree,
            context,
            ProverResult.empty,
            fakeMessage
          )
          .isSuccess shouldBe false

        if (testScalaFunc) contract.scalaFunc(context) shouldEqual buyerPk
    }
  }

  private def testBuyerContractTokens(
    contractSource: BuyerContractSource,
    testScalaFunc: Boolean = true
  ) = {
    val verifier           = new ErgoLikeTestInterpreter
    val prover             = new ContextEnrichingTestProvingInterpreter
    val pubkey             = prover.dlogSecrets.head.publicImage
    val buyerPk: SigmaProp = CSigmaProp(pubkey)

    forAll(tokenIdGen.map(_.toColl), arbLong.arbitrary) {
      case (tokenId, tokenAmount) =>
        val txWithTokens = createTransaction(
          IndexedSeq(
            ErgoBox(
              value               = 1,
              ergoTree            = pubkey,
              creationHeight      = 0,
              additionalTokens    = Seq((Digest32 @@ tokenId.toArray, tokenAmount)),
              additionalRegisters = Map(ErgoBox.R4 -> ByteArrayConstant(fakeSelf.id))
            )
          )
        )

        val contract = contractSource(tokenId, tokenAmount, buyerPk)

        val context = ctx(50, txWithTokens)
        verifier
          .verify(
            contract.ergoTree,
            context,
            ProverResult.empty,
            fakeMessage
          )
          .get
          ._1 shouldEqual true

        if (testScalaFunc)
          contract.scalaFunc(context) shouldEqual CSigmaProp(TrivialProp.TrueProp)
    }

  }

  private def testSellerContractWithBuyer(
    contractSource: SellerContractSource,
    testScalaFunc: Boolean = true
  ) = {
    val verifier                = new ErgoLikeTestInterpreter
    val prover                  = new ContextEnrichingTestProvingInterpreter
    val sellerPk                = prover.dlogSecrets.head.publicImage
    val sellerPkProp: SigmaProp = CSigmaProp(sellerPk)

    forAll(arbLong.arbitrary) {
      case (ergAmount) =>
        val tx = createTransaction(
          IndexedSeq(
            ErgoBox(value = 1, ergoTree = TrivialProp.TrueProp, creationHeight = 0),
            ErgoBox(
              value               = ergAmount,
              ergoTree            = sellerPk,
              creationHeight      = 0,
              additionalRegisters = Map(ErgoBox.R4 -> ByteArrayConstant(fakeSelf.id))
            )
          )
        )
        val contract = contractSource(ergAmount, sellerPkProp)

        val context = ctx(50, tx)
        verifier
          .verify(contract.ergoTree, context, ProverResult.empty, fakeMessage)
          .get
          ._1 shouldBe true

        if (testScalaFunc)
          contract.scalaFunc(context) shouldEqual CSigmaProp(TrivialProp.TrueProp)
    }
  }

  private def testSellerContractCancels(
    contractSource: SellerContractSource,
    testScalaFunc: Boolean = true
  ) = {
    val prover    = new ContextEnrichingTestProvingInterpreter
    val verifier  = new ErgoLikeTestInterpreter
    val ergAmount = 100L
    val pubkey    = prover.dlogSecrets.head.publicImage

    val pk: SigmaProp = CSigmaProp(pubkey)
    val contract      = contractSource(ergAmount, pk)
    val tree          = contract.ergoTree

    val spendingTransaction = createTransaction(
      IndexedSeq(
        ErgoBox(value = 1, ergoTree = TrivialProp.TrueProp, creationHeight = 0),
        // second box as a workaround for costing issue
        // https://github.com/ScorexFoundation/sigmastate-interpreter/issues/628
        ErgoBox(
          value          = 1,
          ergoTree       = TrivialProp.TrueProp, // any address
          creationHeight = 0
        )
      )
    )
    val context = ctx(50, spendingTransaction)

    val pr = prover.prove(tree, context, fakeMessage).get
    verifier.verify(tree, context, pr, fakeMessage).get._1 shouldBe true

    if (testScalaFunc) contract.scalaFunc(context) shouldEqual CSigmaProp(pubkey)
  }

  private def testSellerContractNoBuyer(
    contractSource: SellerContractSource,
    testScalaFunc: Boolean = true
  ) = {
    val verifier            = new ErgoLikeTestInterpreter
    val prover              = new ContextEnrichingTestProvingInterpreter
    val pubkey              = prover.dlogSecrets.head.publicImage
    val sellerPk: SigmaProp = CSigmaProp(pubkey)
    val tx                  = createTransaction(IndexedSeq(ErgoBox(1, TrivialProp.TrueProp, 0)))

    forAll(arbLong.arbitrary) {
      case (ergAmount) =>
        val contract = contractSource(ergAmount, sellerPk)

        val context = ctx(50, tx)
        verifier
          .verify(
            contract.ergoTree,
            context,
            ProverResult.empty,
            fakeMessage
          )
          .isSuccess shouldBe false

        if (testScalaFunc) contract.scalaFunc(context) shouldEqual sellerPk
    }
  }

  def buyerContractExpectedProp(
    buyer: ProveDlog,
    tokenId: Coll[Byte],
    tokenAmount: Long
  ): SigmaPropValue =
    SigmaOr(
      Seq(
        SigmaPropConstant(buyer),
        BinAnd(
          BinAnd(
            GT(
              SizeOf(Outputs),
              IntConstant(0)
            ),
            OptionIsDefined(
              ExtractRegisterAs(
                ByIndex(Outputs, IntConstant(0), None),
                R4,
                SOption(SCollectionType(SByte))
              )
            )
          ),
          BlockValue(
            Vector(
              // tokens
              ValDef(
                1,
                List(),
                ExtractRegisterAs(
                  ByIndex(Outputs, IntConstant(0), None),
                  R2,
                  SOption(SCollectionType(STuple(SCollectionType(SByte), SLong)))
                ).get
              ),
              ValDef(
                2,
                List(),
                BinAnd(
                  BinAnd(
                    GT(
                      SizeOf(
                        ValUse(
                          1,
                          SCollectionType(STuple(SCollectionType(SByte), SLong))
                        )
                      ),
                      IntConstant(0)
                    ),
                    EQ(
                      SelectField(
                        ByIndex(
                          ValUse(
                            1,
                            SCollectionType(STuple(SCollectionType(SByte), SLong))
                          ),
                          IntConstant(0),
                          None
                        ),
                        1
                      ),
                      ByteArrayConstant(tokenId)
                    )
                  ),
                  GE(
                    SelectField(
                      ByIndex(
                        ValUse(
                          1,
                          SCollectionType(STuple(SCollectionType(SByte), SLong))
                        ),
                        IntConstant(0),
                        None
                      ),
                      2
                    ),
                    LongConstant(tokenAmount)
                  )
                )
              ),
              ValDef(
                3,
                List(),
                EQ(
                  ExtractRegisterAs(
                    ByIndex(Outputs, IntConstant(0), None),
                    R4,
                    SOption(SCollectionType(SByte))
                  ).get,
                  ExtractId(Self)
                )
              )
            ),
            BinAnd(
              BinAnd(
                ValUse(2, SBoolean),
                EQ(
                  ExtractScriptBytes(ByIndex(Outputs, IntConstant(0), None)),
                  SigmaPropBytes(SigmaPropConstant(buyer))
                )
              ),
              ValUse(3, SBoolean)
            )
          ).asInstanceOf[Value[SBoolean.type]]
        ).toSigmaProp
      )
    )

  def sellerContractExpectedProp(seller: ProveDlog, ergAmount: Long): SigmaPropValue =
    SigmaOr(
      Seq(
        SigmaPropConstant(seller),
        BoolToSigmaProp(
          BinAnd(
            BinAnd(
              GT(SizeOf(Outputs), IntConstant(1)),
              OptionIsDefined(
                ExtractRegisterAs(
                  ByIndex(Outputs, IntConstant(1), None),
                  R4,
                  SOption(SCollectionType(SByte))
                )
              )
            ),
            BlockValue(
              Vector(
                ValDef(
                  1,
                  List(),
                  EQ(
                    ExtractRegisterAs(
                      ByIndex(Outputs, IntConstant(1), None),
                      R4,
                      SOption(SCollectionType(SByte))
                    ).get,
                    ExtractId(Self)
                  )
                )
              ),
              BinAnd(
                BinAnd(
                  GE(
                    ExtractAmount(ByIndex(Outputs, IntConstant(1), None)),
                    LongConstant(ergAmount)
                  ),
                  ValUse(1, SBoolean)
                ),
                EQ(
                  ExtractScriptBytes(ByIndex(Outputs, IntConstant(1), None)),
                  SigmaPropBytes(SigmaPropConstant(seller))
                )
              )
            ).asInstanceOf[Value[SBoolean.type]]
          )
        )
      )
    )

  property("buyer contract(method call): ergo tree") {
    forAll(tokenIdGen.map(_.toColl), arbLong.arbitrary, proveDlogGen) {
      case (tokenId, tokenAmount, proveDlogPk) =>
        val pk: SigmaProp = CSigmaProp(proveDlogPk)
        val c =
          AssetsAtomicExchangeCompilation.buyerContractInstance(tokenId, tokenAmount, pk)
        val expectedProp = buyerContractExpectedProp(proveDlogPk, tokenId, tokenAmount)
        c.prop shouldEqual expectedProp
    }
  }

  property("buyer contract verified(method call): ergo tree") {
    forAll(tokenIdGen.map(_.toColl), arbLong.arbitrary, proveDlogGen) {
      case (tokenId, tokenAmount, proveDlogPk) =>
        val pk: SigmaProp = CSigmaProp(proveDlogPk)
        val c =
          AssetsAtomicExchangeVerifiedCompilationConverted
            .buyerContractInstanceNonVerifiedTypes(tokenId, tokenAmount, pk)
        val expectedProp = buyerContractExpectedProp(proveDlogPk, tokenId, tokenAmount)
        c.prop shouldEqual expectedProp
    }
  }

  property("buyer contract(body): ergo tree") {
    forAll(tokenIdGen.map(_.toColl), arbLong.arbitrary, proveDlogGen) {
      case (tokenId, tokenAmount, proveDlogPk) =>
        val pk: SigmaProp = CSigmaProp(proveDlogPk)
        val contract =
          AssetsAtomicExchangeBodyCompilation.buyerContract(tokenId, tokenAmount, pk)
        val expectedProp = buyerContractExpectedProp(proveDlogPk, tokenId, tokenAmount)
        contract.prop shouldEqual expectedProp
    }
  }

  property("seller contract(body): ergo tree") {
    forAll(arbLong.arbitrary, proveDlogGen) {
      case (ergAmount, proveDlogPk) =>
        val pk: SigmaProp = CSigmaProp(proveDlogPk)
        val contract      = AssetsAtomicExchangeBodyCompilation.sellerContract(ergAmount, pk)
        val expectedProp  = sellerContractExpectedProp(proveDlogPk, ergAmount)
        contract.prop shouldEqual expectedProp
    }
  }

  property("seller contract(method call): ergo tree") {
    forAll(arbLong.arbitrary, proveDlogGen) {
      case (ergAmount, proveDlogPk) =>
        val pk: SigmaProp = CSigmaProp(proveDlogPk)
        val c             = AssetsAtomicExchangeCompilation.sellerContractInstance(ergAmount, pk)
        val expectedProp  = sellerContractExpectedProp(proveDlogPk, ergAmount)
        assert(c.prop == expectedProp)
    }
  }

  property("seller contract verified(method call): ergo tree") {
    forAll(arbLong.arbitrary, proveDlogGen) {
      case (ergAmount, proveDlogPk) =>
        val pk: SigmaProp = CSigmaProp(proveDlogPk)
        val c = AssetsAtomicExchangeVerifiedCompilationConverted
          .sellerContractInstanceNonVerifiedTypes(ergAmount, pk)
        val expectedProp = sellerContractExpectedProp(proveDlogPk, ergAmount)
        assert(c.prop == expectedProp)
    }
  }

  property("buyer contract(body), buyer claim") {
    testBuyerContractCancels(
      AssetsAtomicExchangeBodyCompilation.buyerContract,
      testScalaFunc = false
    )
  }

  property("buyer contract(method call), buyer claim") {
    testBuyerContractCancels(
      AssetsAtomicExchangeCompilation.buyerContractInstance
    )
  }

  property("buyer contract verified(method call), buyer claim") {
    testBuyerContractCancels(
      AssetsAtomicExchangeVerifiedCompilationConverted.buyerContractInstanceNonVerifiedTypes
    )
  }

  property("buyer contract(body), no tokens") {
    testBuyerContractNoTokens(
      AssetsAtomicExchangeBodyCompilation.buyerContract,
      testScalaFunc = false
    )
  }

  property("buyer contract(method call), no tokens") {
    testBuyerContractNoTokens(AssetsAtomicExchangeCompilation.buyerContractInstance)
  }

  property("buyer contract verified(method call), no tokens") {
    testBuyerContractNoTokens(
      AssetsAtomicExchangeVerifiedCompilationConverted.buyerContractInstanceNonVerifiedTypes
    )
  }

  property("buyer contract(body), tokens") {
    testBuyerContractTokens(
      AssetsAtomicExchangeBodyCompilation.buyerContract,
      testScalaFunc = false
    )
  }

  property("buyer contract(method call), tokens") {
    testBuyerContractTokens(AssetsAtomicExchangeCompilation.buyerContractInstance)
  }

  property("buyer contract verified(method call), tokens") {
    testBuyerContractTokens(
      AssetsAtomicExchangeVerifiedCompilationConverted.buyerContractInstanceNonVerifiedTypes
    )
  }

  property("seller contract(method call), seller claim") {
    testSellerContractCancels(AssetsAtomicExchangeCompilation.sellerContractInstance)
  }

  property("seller contract(body), seller claim") {
    testSellerContractCancels(
      AssetsAtomicExchangeBodyCompilation.sellerContract,
      testScalaFunc = false
    )
  }

  property("seller contract verified(method call), seller claim") {
    testSellerContractCancels(
      AssetsAtomicExchangeVerifiedCompilationConverted.sellerContractInstanceNonVerifiedTypes
    )
  }

  property("seller contract(method call), no buyer") {
    testSellerContractNoBuyer(AssetsAtomicExchangeCompilation.sellerContractInstance)
  }

  property("seller contract verified(method call), no buyer") {
    testSellerContractNoBuyer(
      AssetsAtomicExchangeVerifiedCompilationConverted.sellerContractInstanceNonVerifiedTypes
    )
  }

  property("seller contract(body), no buyer") {
    testSellerContractNoBuyer(
      AssetsAtomicExchangeBodyCompilation.sellerContract,
      testScalaFunc = false
    )
  }

  property("seller contract(method call), with buyer") {
    testSellerContractWithBuyer(AssetsAtomicExchangeCompilation.sellerContractInstance)
  }

  property("seller contract verified(method call), with buyer") {
    testSellerContractWithBuyer(
      AssetsAtomicExchangeVerifiedCompilationConverted.sellerContractInstanceNonVerifiedTypes
    )
  }

  property("seller contract(body), with buyer") {
    testSellerContractWithBuyer(
      AssetsAtomicExchangeBodyCompilation.sellerContract,
      testScalaFunc = false
    )
  }
}
