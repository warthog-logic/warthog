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

package org.warthog.fol.datastructures.cnf

import org.warthog.generic.datastructures.cnf.{ImmutableClause, ClauseLike}
import org.warthog.fol.formulas.FOL

/**
 * File Description
 *
 * Author: zengler
 * Date:   16.05.12
 */
class ImmutableFOLClause(ls: List[FOLLiteral]) extends ImmutableClause[FOL, FOLLiteral](ls) {

  def this() {
    this(Nil)
  }

  def this(lits: FOLLiteral*) {
    this(lits.toList)
  }

  def this(c: ClauseLike[FOL, FOLLiteral]) {
    this(c.literals)
  }

  /**
   * Delete a literal in this clause
   * @param lit a literal
   */
  def delete(lit: FOLLiteral) = new ImmutableFOLClause(_lits.filterNot(_ == lit))

  /**
   * Push a literal to this clause
   * @param lit a literal
   */
  def push(lit: FOLLiteral) =
    if (!_lits.contains(lit))
      new ImmutableFOLClause(lit :: _lits)
    else
      this

  /**
   * Add a number of literals to this clause
   * @param lits a list of literals
   */
  def pushLiterals(lits: FOLLiteral*) = new ImmutableFOLClause((_lits ++ lits).distinct)

}
