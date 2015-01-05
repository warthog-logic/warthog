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
 * Trait for converting pseudo boolean constraints (PBC) to a Boolean formula (SAT).
 *
 * Note: A object/class which implements this trait has to override at least le() or ge().
 */
trait PBCtoSAT {

  def le(weights: List[(Int, Lit)], bound: Int, prefix: String = PBCtoSAT.DEFAULT_PREFIX): Set[Clause] =
    ge(weights.map {
      case (coeff, lit) => (coeff, lit.negate)
    }, PBCtoSAT.sumWeights(weights) - bound, prefix)

  def ge(weights: List[(Int, Lit)], bound: Int, prefix: String = PBCtoSAT.DEFAULT_PREFIX): Set[Clause] =
    le(weights.map {
      case (coeff, lit) => (coeff, lit.negate)
    }, PBCtoSAT.sumWeights(weights) - bound, prefix)

  def eq(weights: List[(Int, Lit)], bound: Int, prefix: String = PBCtoSAT.DEFAULT_PREFIX) =
    le(weights, bound, prefix + "_le") ++ ge(weights, bound, prefix + "_ge")

  def lt(weights: List[(Int, Lit)], bound: Int, prefix: String = PBCtoSAT.DEFAULT_PREFIX) =
    le(weights, bound - 1, prefix)

  def gt(weights: List[(Int, Lit)], bound: Int, prefix: String = PBCtoSAT.DEFAULT_PREFIX) =
    ge(weights, bound + 1, prefix)

}

object PBCtoSAT {

  val DEFAULT_PREFIX = "D_"

  /**
   * Normalizes a constraint to a form in which all weights are positive
   *
   * @param weights list of pairs with weight and corresponding variable
   * @param bound the maximum bound
   * @return the normalized weights and bound as a pair
   */
  def normalize(weights: List[(Int, Lit)], bound: Int): (List[(Int, Lit)], Int) = {
    if (!weights.isEmpty) {
      val (nWeights, nBound) = normalize(weights.tail, bound)
      val head = weights.head
      if (head._1 < 0) {
        ((-head._1, head._2.negate) :: nWeights, nBound - head._1)
      } else {
        (head :: nWeights, nBound)
      }
    } else {
      (List(), bound)
    }
  }

  def sumWeights(l: List[(Int, Lit)]) = l.unzip._1.sum
}