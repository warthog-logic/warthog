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

package org.warthog.fol.transformations

import org.warthog.generic.transformations.{Substitution => GenericSubstitution}
import org.warthog.fol.formulas._
import org.warthog.generic.formulas._

/**
 * Substitution for FOL generic
 *
 * Author: zengler
 * Date:   19.01.12
 */
trait Substitution extends GenericSubstitution[FOL] {

  /**
   * Substitute a term for a variable
   * @param v: the variable
   * @param t: the term to substitute
   * @return the substituted formula f[v/t]
   */
  def substitute(v: FOLVariable, t: FOLTerm): Formula[FOL] = subst(Map(v -> t), f)

  /**
   * Substitute a term for a variable
   * @param m: a map of variables to terms
   * @return the substituted formula
   */
  def substitute(m: Map[FOLVariable, FOLTerm]) = subst(m, f)

  private def subst(m: Map[FOLVariable, FOLTerm], arg: Formula[FOL]): Formula[FOL] = arg match {
    case p: FOLPredicate   => p.tsubst(m)
    case q: FOLForAll      => q.tsubst(m)
    case q: FOLExists      => q.tsubst(m)
    case Not(p)            => Not(subst(m, p))
    case Implication(p, q) => Implication(subst(m, p), subst(m, q))
    case Equiv(p, q)       => Equiv(subst(m, p), subst(m, q))
    case Xor(p, q)         => Xor(subst(m, p), subst(m, q))
    case And(ps@_*)        => And(ps.map(subst(m, _)): _*)
    case Or(ps@_*)         => Or(ps.map(subst(m, _)): _*)
    case _                 => arg
  }
}


