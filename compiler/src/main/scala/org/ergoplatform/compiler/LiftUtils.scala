package org.ergoplatform.compiler

import scalan.RType
import sigmastate.{SCollection, SCollectionType, STuple, SType, Values}
import sigmastate.Values.{
  CollectionConstant,
  ConcreteCollection,
  Constant,
  EvaluatedValue
}
import sigmastate.eval.Evaluation.rtypeToSType
import sigmastate.lang.{DefaultSigmaBuilder, SigmaBuilder}
import special.collection.{Coll, CollOverArrayBuilder}

object LiftUtils {

  val builder: SigmaBuilder = DefaultSigmaBuilder

  def constValToErgoTree[A: RType](v: A): EvaluatedValue[SType] =
    implicitly[RType[A]] match {
      case special.collection.CollType(_) => collToErgoTree(v.asInstanceOf[Coll[_]])
      case pt: RType.PairType[a, b] =>
        tuple2ToErgoTree(v.asInstanceOf[(a, b)], pt.tFst, pt.tSnd)
      case RType.PrimitiveType(_) =>
        builder.liftAny(v).get.asInstanceOf[EvaluatedValue[SType]]
    }

  def tuple2ToErgoTree[A, B](
    t: (A, B),
    tA: RType[A],
    tB: RType[B]
  ): EvaluatedValue[STuple] =
    Values.Tuple(constValToErgoTree(t._1)(tA), constValToErgoTree(t._2)(tB))

  def collToErgoTree[A](a: Coll[A]): EvaluatedValue[SCollection[SType]] =
    a.tItem match {
      case special.collection.CollType(tA) =>
        val c = a.asInstanceOf[Coll[Coll[Any]]].toArray.map { x =>
          collToErgoTree(x)
        }
        ConcreteCollection(c, SCollectionType(rtypeToSType(tA)))
      case tA @ RType.PrimitiveType(_) =>
        val st = rtypeToSType(tA)
        val c = (new CollOverArrayBuilder)
          .fromArray(a.toArray)(tA)
          .asInstanceOf[special.collection.Coll[st.type#WrappedType]]
        CollectionConstant[st.type](c, st).asInstanceOf[Constant[SCollection[SType]]]
      case pt: RType.PairType[a, b] =>
        val c = a
          .asInstanceOf[Coll[(a, b)]]
          .toArray
          .map(tuple2ToErgoTree(_, pt.tFst, pt.tSnd))
        val tpe = STuple(rtypeToSType(pt.tFst), rtypeToSType(pt.tSnd))
        ConcreteCollection(c, tpe)
    }

}
