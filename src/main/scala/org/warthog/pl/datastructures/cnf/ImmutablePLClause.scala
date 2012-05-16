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
import org.warthog.generic.datastructures.cnf.{ClauseLike, ImmutableClause}

/**
 * Representation of an immutable propositional clause
 *
 * Author: zengler
 * Date:   14.05.12
 */
class ImmutablePLClause(ls: List[PLLiteral]) extends ImmutableClause[PL, PLLiteral](ls) {
  def this() {
    this(Nil)
  }

  def this(lits: PLLiteral*) {
    this(lits.toList)
  }

  def this(c: ClauseLike[PL, PLLiteral]) {
    this(c.literals)
  }

  /**
   * Delete a literal in this clause
   * @param lit a literal
   */
  def delete(lit: PLLiteral) = new ImmutablePLClause(_lits.filterNot(_ == lit))

  /**
   * Push a literal to this clause
   * @param lit a literal
   */
  def push(lit: PLLiteral) =
    if (!_lits.contains(lit))
      new ImmutablePLClause(lit :: _lits)
    else
      this

  /**
   * Add a number of literals to this clause
   * @param lits a list of literals
   */
  def pushLiterals(lits: PLLiteral*) = new ImmutablePLClause((_lits ++ lits).distinct)
}
