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

import org.warthog.pl.formulas.{PLAtom, PL}
import org.warthog.generic.formulas.{Not, Formula}
import org.warthog.generic.datastructures.cnf.Literal

/**
 * Representation of a propositional literal
 *
 * Author: zengler
 * Date:   15.05.12
 */
case class PLLiteral(v: PLAtom, phase: Boolean = true) extends Literal[PL] {

  override def toString = if (phase) v.toString else Formula.NOT + v

  /**
   * A formula representation of the literal
   * @return a formula respresentation in propositional logic
   */
  def toFormula: Formula[PL] = if (phase) v else -v

  /**
   * Return a negated copy of the literal
   * @return a negated copy of the literal
   */
  def negate: PLLiteral = PLLiteral(v, !phase)

}

object PLLiteral {
  def apply(f: Formula[PL]): PLLiteral = f match {
    case v: PLAtom      => new PLLiteral(v)
    case Not(v: PLAtom) => new PLLiteral(v, false)
    case _              => throw new IllegalArgumentException("Cannot convert %s to a literal.".format(f))
  }

  def apply(s: String, phase: Boolean): PLLiteral = new PLLiteral(PLAtom(s), phase)
}
