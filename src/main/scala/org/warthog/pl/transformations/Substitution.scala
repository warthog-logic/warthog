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

package org.warthog.pl.transformations

import org.warthog.generic.formulas._
import org.warthog.generic.transformations.{Substitution => GenericSubstituion}
import org.warthog.pl.formulas.{PL, PLAtom}
import org.warthog.generic.formulas.{And, Formula, Not, Or}

/**
 * Substitution for propositional generic
 *
 * Author: zengler
 * Date:   19.01.12
 */
trait Substitution extends GenericSubstituion[PL] {

  /**
   * Substitute a formula for a propositional atom
   * @param v: the propositional atom
   * @param t: the formula to substitute
   * @return the substituted formula f[v/t]
   */
  def substitute(v: PLAtom, t: Formula[PL]): Formula[PL] = subst(Map(v -> t), f)

  /**
   * Substitute a formula for a propositional atom
   * @param m: a map of atoms to generic
   * @return the substituted formula
   */
  def substitute(m: Map[PLAtom, Formula[PL]]): Formula[PL] = subst(m, f)

  private def subst(m: Map[PLAtom, Formula[PL]], arg: Formula[PL]): Formula[PL] = arg match {
    case p: PLAtom         => m.getOrElse(p, p)
    case Not(p)            => Not(subst(m, p))
    case Implication(p, q) => Implication(subst(m, p), subst(m, q))
    case Equiv(p, q)       => Equiv(subst(m, p), subst(m, q))
    case Xor(p, q)         => Xor(subst(m, p), subst(m, q))
    case And(ps@_*)        => And(ps.map(subst(m, _)): _*)
    case Or(ps@_*)         => Or(ps.map(subst(m, _)): _*)
    case _                 => arg
  }
}
