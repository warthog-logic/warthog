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

package org.warthog.pl.decisionprocedures.satsolver

import org.warthog.generic.formulas.{Formula, Falsum}
import org.warthog.pl.formulas.PL

/**
 * Common interface for SAT solvers
 *
 * Author: kuebler
 * Date:   25.01.12
 */
trait Solver {
  /**
   * Add a formula to the solver.  If the solver held a formula `F` bevor, it now holds `F /\ fm`.
   * @param fm the formula to add
   */
  def add(fm: Formula[PL]): Unit

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
  def mark(): Unit

  /**
   * Undo all the additions until the last marked position.
   */
  def undo(): Unit

  /**
   * Check decisionprocedures satsolver of the formula on the internal stack.
   * @param timeout a timeout value for the solver
   * @return <0: UNSAT, >0: SAT, 0: UNKNOWN
   */
  def sat(timeout: Duration): Int

  /**
   * Get a model of the formula
   * @return [[org.warthog.formulas.Falsum]] if UNSAT, else satisfying assignment
   *           in form of a conjunction of literals
   */
  def getModel(): Formula[PL]

  /**
   * Reset the solver
   */
  def reset(): Unit

  /**
   * Initialize the solver
   */
  def init(): Unit
}

object Solver {
  /**
   * literal: true
   */
  val TRUE_LITERAL: String = "T"
  /**
   * literal: false
   */
  val FALSE_LITERAL: String = "F"
  /**
   * minimum position model type
   */
  val Modeltype_MinPos: Int = 0
  /**
   * maximum position model type
   */
  val Modeltype_MaxPos: Int = 1
  /**
   * minimum negation model type
   */
  val Modeltype_MinNeg: Int = 2
  /**
   * maximum negation model type
   */
  val Modeltype_MaxNeg: Int = 3
  /**
   * minimum redundant codes
   */
  val MIN_REDUNDANT: Int = Integer.MIN_VALUE
}

/**
 * Solvers that support unsat core extraction mix-in this trait
 */
trait UnsatCore {
  this: Solver =>
  def unsatCore(): Formula[PL]
}

/**
 * Solvers which support proof trace generation mix-in this trait
 */
trait ProofTrace {
  /* TODO: Suitable form for proof traces/refutations? */
}

/**
 * Solvers which support interpolant generation mix-in this trait
 *
 * There are solvers that generate Craig interpolants on the fly, if a
 * solver supports proof trace generation, interpolants can, however, be
 * easily extracted from resolution refutations (for an overview cf.
 * D'Silva, Kroening et al.: "Interpolant Strength", VMCAI 2010).
 */
trait CraigInterpolant {
  this: ProofTrace =>
  def getInterpolant(): Formula[PL]
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
    def apply(f: Solver => Unit) = {
      s.init()
      try {
        f(s)
      } finally {
        s.reset()
      }
    }
  }

  def apply(s: Solver) = {
    new SolverExecutor(s)
  }
}

class Duration(val to: Long = 0) {
  def ms = new Duration(to * 1000)

  def s = new Duration(to * 1000000)

  def min = new Duration(to * 1000000 * 60)

  def h = new Duration(to * 1000000 * 60 * 60)

  def d = new Duration(to * 1000000 * 60 * 60 * 24)
}

case object Infinity extends Duration

/*
class MockSolver extends Solver {
  override def add(fm: Formula): Unit = println("add")
  override def mark(): Unit = println("mark")
  override def undo(): Unit = println("undo")
  override def sat(timeout: Duration): Boolean = { println("sat"); false }
  override def reset(): Unit = println("reset")
  override def init(): Unit = println("init")
}

object test {
  def main(args: Array[String]) = {
    sat(new MockSolver) {
      solver => {
        solver.sat(5.s)
      }
    }
  }
}
*/
