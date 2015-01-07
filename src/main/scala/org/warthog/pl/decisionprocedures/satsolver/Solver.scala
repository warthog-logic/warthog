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

package org.warthog.pl.decisionprocedures.satsolver

import org.warthog.generic.formulas.{Formula, Falsum}
import org.warthog.pl.formulas.PL
import org.warthog.pl.datastructures.cnf.ImmutablePLClause

/**
 * Common interface for SAT solvers
 */
trait Solver {
  /**
   * Reset the solver
   */
  def reset()

  /**
   * Add a formula to the solver.  If the solver held a formula `F` before, it now holds `F /\ fm`.
   * @param fm the formula to add
   */
  def add(fm: Formula[PL])

  /**
   * Add a clause to the solver.
   * @param clause the clause to add
   */
  def add(clause: ImmutablePLClause)

  /**
   * Mark a solver's internal stack position.  Executing
   * {{{
   * Solver.add(f0)
   * Solver.mark()
   * Solver.add(a1)
   * Solver.add(a2)
   * Solver.undo()
   * }}}
   * will set the solver back into the state after adding `f0`
   */
  def mark()

  /**
   * Undo all the additions until the last marked position.
   */
  def undo()

  /**
   * Checks the previously added constraints for satisfiability.
   * @return Appropriate constant UNKOWN, SAT or UNSAT
   */
  def sat(): Int

  def getModel(): Option[Model]
}

object Solver {
  /* Possible solver states */
  final val UNKNOWN = 0
  final val SAT = 1
  final val UNSAT = -1
}

/**
 * Example usage:
 *
 * {{{
 * sat(new PicoSat) {
 *   solver => {
 *     solver.add(...);
 *     solver.sat(Infinity);
 * }
 * }
 * }}}
 */
object sat {

  class SolverExecutor(s: Solver) {
    def apply(f: Solver => Unit) {
      s.reset()
      f(s)
    }
  }

  def apply(s: Solver) = {
    new SolverExecutor(s)
  }
}
