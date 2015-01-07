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

  def le(weights: List[(Long, Lit)], bound: Long, prefix: String = PBCtoSAT.DEFAULT_PREFIX): Set[Clause] =
    ge(weights.map {
      case (coeff, lit) => (coeff, lit.negate)
    }, PBCtoSAT.sumWeights(weights) - bound, prefix)

  def ge(weights: List[(Long, Lit)], bound: Long, prefix: String = PBCtoSAT.DEFAULT_PREFIX): Set[Clause] =
    le(weights.map {
      case (coeff, lit) => (coeff, lit.negate)
    }, PBCtoSAT.sumWeights(weights) - bound, prefix)

  def eq(weights: List[(Long, Lit)], bound: Long, prefix: String = PBCtoSAT.DEFAULT_PREFIX) =
    le(weights, bound, prefix + PBCtoSAT.DEFAULT_ADDITIONAL_LE_PREFIX) ++
      ge(weights, bound, prefix + PBCtoSAT.DEFAULT_ADDITIONAL_GE_PREFIX)

  def lt(weights: List[(Long, Lit)], bound: Long, prefix: String = PBCtoSAT.DEFAULT_PREFIX) =
    le(weights, bound - 1, prefix)

  def gt(weights: List[(Long, Lit)], bound: Long, prefix: String = PBCtoSAT.DEFAULT_PREFIX) =
    ge(weights, bound + 1, prefix)
}

object PBCtoSAT {

  val DEFAULT_PREFIX = "D_"
  val DEFAULT_ADDITIONAL_LE_PREFIX = "_le_"
  val DEFAULT_ADDITIONAL_GE_PREFIX = "_ge_"

  /**
   * Normalizes a lower-equal PBC constraint to a form in which all terms are positive.
   *
   * @param terms list of terms (coefficient and literal)
   * @param bound the maximum bound
   * @return the normalized terms and bound
   */
  def normalize(terms: List[(Long, Lit)], bound: Long): (List[(Long, Lit)], Long) =
    terms match {
      case head :: tail => {
        val (nWeights, nBound) = normalize(terms.tail, bound)
        val head = terms.head
        if (head._1 < 0) {
          ((-head._1, head._2.negate) :: nWeights, nBound - head._1)
        } else {
          (head :: nWeights, nBound)
        }
      }
      case Nil => (terms, bound)
    }

  def sumWeights(l: List[(Long, Lit)]) = l.unzip._1.sum
}