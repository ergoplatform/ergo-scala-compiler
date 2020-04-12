package org.ergoplatform.compiler.dsl

import org.ergoplatform.compiler.ErgoContract
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.{SigmaCompiler, TransformingSigmaBuilder}
import org.ergoplatform.ErgoAddressEncoder.MainnetNetworkPrefix
import sigmastate.eval.CompiletimeIRContext
import sigmastate.eval.Evaluation
import sigmastate.SType
import sigmastate.SType.AnyOps

trait ErgoScriptCompilationDsl {

  private val compiler = SigmaCompiler(MainnetNetworkPrefix, TransformingSigmaBuilder)

  implicit private var IR: CompiletimeIRContext = new CompiletimeIRContext()

  private def compile(env: ScriptEnv, ergoScript: String): ErgoContract = {
    val liftedEnv = env.mapValues { v =>
      val tV      = Evaluation.rtypeOf(v).get
      val elemTpe = Evaluation.rtypeToSType(tV)
      IR.builder.mkConstant[SType](v.asWrappedType, elemTpe)
    }
    val prop = compiler.compile(liftedEnv, ergoScript)
    ErgoContract(_ => ???, prop)
  }

  def contract(env: ScriptEnv, ergoScript: String): ErgoContract =
    compile(env, ergoScript)

}
