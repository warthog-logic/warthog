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

/**
 * CNF-encoding of pseudo-Boolean constraints
 *
 * O. Bailleux, Y. Boufkhad, O. Roussel:
 * "A Translation of Pseudo-Boolean Constraints to SAT",
 * JSAT 2(1-4):191-200 (2006)
 *
 * Remark: In the worst case, the size of the produced formula can be exponentially related
 * to the size of the input constraint, but Boolean cardinality constraints (and some other
 * classes) are encoded in polynomial time and size.
 *
 */
object BailleuxBoufkhadRoussel extends PBCtoSAT {

  override def le(weights: List[(Int, Lit)], bound: Int, prefix: String = "D_"): Set[Clause] = {
    val (nWeights, nBound) = PBCtoSAT.normalize(weights, bound)
    new BailleuxBoufkhadRousselHelper(nWeights, nBound, prefix).le()
  }
}

private class BailleuxBoufkhadRousselHelper(ws: List[(Int, Lit)], bound: Int, prefix: String) {

  private val weights: List[(Int, Lit)] = ws.sortBy(_._1)

  private def isTerminal(i: Int, b: Int): Boolean = (b <= 0) || (PBCtoSAT.sumWeights(weights.take(i)) <= b)

  private def varName(i: Int, b: Int): String = s"$prefix${i}_$b"

  /**
   * Method is assuming there are no fixed literals! (=> all literals are free)
   *
   */
  def le() = {
    if (PBCtoSAT.sumWeights(weights) <= bound) Set.empty[Clause]
    else leWorker(weights.length, bound)()._2 + new Clause(Lit(varName(weights.length, bound), true))
  }

  implicit def toLit(s: String) = Lit(s, true)

  type T = (Set[String], Set[Clause])

  private def leWorker(i: Int, b: Int)(state: T = (Set.empty[String], Set.empty[Clause])): T = {
    val (used, clauses) = state
    if (used contains varName(i, b))
      (used, clauses)
    else if (!isTerminal(i, b)) {
      val dib = varName(i, b)
      val di1bw = varName(i - 1, b - weights(i - 1)._1)
      val di1b = varName(i - 1, b)
      val xi = weights(i - 1)._2
      val newElems = Set(new Clause(di1bw.negate, dib),
        new Clause(dib.negate, di1b),
        new Clause(dib.negate, xi.negate, di1bw),
        new Clause(di1b.negate, xi, dib))
      (leWorker(i - 1, b) _ andThen leWorker(i - 1, b - weights(i - 1)._1) _)(used + dib, newElems union clauses)
    } else if (b == 0) {
      val di0 = varName(i, 0)
      val xjs = weights.take(i).unzip._2
      (used, clauses union xjs.toSet.map((x: Lit) => new Clause(di0.negate, x.negate)) + new Clause(Lit(di0, true) :: xjs))
    } else if (b < 0) {
      (used, clauses + new Clause(varName(i, b).negate))
    } else {
      (used, clauses + new Clause(varName(i, b)))
    }
  }
}