/*
 * Copyright (c) 2011-2014, Andreas J. Kuebler & Christoph Zengler
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.warthog.pl.generators.pbc


import org.warthog.pl.datastructures.cnf.{ImmutablePLClause => Clause, PLLiteral => Lit}
import org.warthog.generic.formulas._

/**
  * Trait for pseudo boolean constraints
  */
trait PBCtoSAT {

  def le(weights: List[Tuple2[Int,Lit]], bound: Int, prefix: String = "D_"): List[Clause] =
    ge(weights.map(pair => (pair._1, pair._2.negate)), PBCtoSAT.sumWeights(weights) - bound, prefix)

  def ge(weights: List[Tuple2[Int,Lit]], bound: Int, prefix: String = "D_"): List[Clause] =
    le(weights.map(pair => (pair._1, pair._2.negate)), PBCtoSAT.sumWeights(weights) - bound, prefix)

  def eq(weights: List[Tuple2[Int,Lit]], bound: Int, prefix: String = "D_") = le(weights, bound, prefix+"_le") ++ ge(weights,bound, prefix+"_ge")

  def lt(weights: List[Tuple2[Int,Lit]], bound: Int, prefix: String = "D_") = le(weights, bound - 1, prefix)

  def gt(weights: List[Tuple2[Int,Lit]], bound: Int, prefix: String = "D_") = ge(weights, bound + 1, prefix)

}

object PBCtoSAT {

  def sumWeights(l: List[Tuple2[Int,Lit]]) = l.foldRight(0)((pair,r) => pair._1 + r)

}