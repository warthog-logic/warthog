/**
 * Copyright (c) 2011, Andreas J. Kuebler & Christoph Zengler
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

package org.warthog.pl.datastructures.cnf

import org.warthog.pl.formulas.PL
import org.warthog.generic.formulas.{Or, Falsum, Formula}
import org.warthog.generic.datastructures.cnf.{Literal, ClauseLike}

/**
 * Trait for propositional clauses
 *
 * Author: zengler
 * Date:   15.05.12
 */
trait PLClauseLike[T <: Literal[PL]] extends ClauseLike[PL, T] {

  /**
   * A formula representation of the clause
   * @return a formula respresentation in propositional logic
   */
  def toFormula: Formula[PL] =
    if (isEmpty)
      new Falsum
    else if (isUnit)
      literals(0).toFormula
    else
      Or(literals.map(_.toFormula): _*)

  /**
   * Return the premise of the clause as a list of literals
   * @return the premise of the clause
   */
  def premise = literals.filter(!_.phase).toList

  /**
   * Return the consequence of the clause as a list of literals
   * @return the consequence of the clause
   */
  def consequence = literals.filter(_.phase).toList

  /**
   * Is this clause a tautology?
   * @return `true` if the clause is a tautology, `false` otherwise
   */
  def isTautology: Boolean = {
    val (pos, neg) = literals.partition(_.phase)
    for (p <- pos) {
      if (neg.contains(p.negate))
        return true
    }
    false
  }
}
