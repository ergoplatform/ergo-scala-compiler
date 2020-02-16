package org.ergoplatform.compiler.test

object BodySyntax extends EmptyContext with EmptySigmaContract {

  type Coll[A]   = special.collection.Coll[A]
  type SigmaProp = special.sigma.SigmaProp

}
