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

package org.warthog.pl.formulas

import org.warthog.pl.decisionprocedures.TruthTable
import org.warthog.generic.formulas._
import org.warthog.generic.transformations.{RemoveBooleanConstants, CNFDNF}
import org.warthog.generic.printer.PrettyPrinter
import org.warthog.pl.transformations.{DefinitionalCNF, Substitution}

/**
 * Rich formula for propositional logic
 */
trait PLTransformations extends CNFDNF[PL] with Substitution with DefinitionalCNF

trait PLSimplifications extends RemoveBooleanConstants[PL]

class PLFormula(override val f: Formula[PL]) extends PLTransformations with PLSimplifications {

  /**
   * pretty print a formula
   * @param prettyPrinter the pretty printer to use
   * @return the pretty printed formula
   */
  def pp(implicit prettyPrinter: PrettyPrinter[PL]) = prettyPrinter.print(f)

  /**
   * Get the truth table of `f`
   * @return a string representation of `f`'s truth table
   */
  def truthTable = TruthTable.generate(f)

  /**
   * Evaluate a propositional formula
   * @param v the mapping of variables to Boolean values
   * @param arg the formula to evaluate (default `f`)
   * @return `true` if arg evalutes to true under v, `false` otherwise
   */
  def eval(v: Map[PLAtom, Boolean], arg: Formula[PL] = f): Boolean = arg match {
    case f: Falsum[PL]     => false
    case v: Verum[PL]      => true
    case p: PLAtom         => v.get(p) match {
      case Some(t) => t
      case None    => throw new Exception("Variable %s was not assigned".format(p))
    }
    case Not(p)            => !eval(v, p)
    case Implication(p, q) => eval(v, -p || q)
    case Xor(p, q)         => eval(v, p) != eval(v, q)
    case Equiv(p, q)       => eval(v, p) == eval(v, q)
    case And(ps@_*)        => ps.forall(eval(v, _))
    case Or(ps@_*)         => ps.exists(eval(v, _))
  }
}
