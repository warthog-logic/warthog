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

package org.warthog.pl.decisionprocedures.dpll

import org.warthog.pl.formulas.PL
import org.warthog.generic.formulas.Formula
import org.warthog.pl.io.CNFUtil
import org.warthog.pl.datastructures.cnf.{PLLiteral, ImmutablePLClause => Clause}

/**
 * A basic implementation of the DP and DPLL procedures
 *
 * Author: zengler
 * Date:   09.05.12
 */
object DPLL {

  /**
   * Main entry point for the DP procedure
   * @param f a PL formula
   * @return `true` if the clauseset is satisfiable, `false` otherwise
   */
  def dp(f: Formula[PL]): Boolean = dpRec(CNFUtil.toCNF(f))

  /**
   * The main recursive DP algorithm
   * @param clauses a list of clauses
   * @return `true` if the clauseset is satisfiable, `false` otherwise
   */
  private def dpRec(clauses: List[Clause]): Boolean = {
    if (clauses.isEmpty)
      true
    else if (clauses.find(_.size == 0) != None)
      false
    else {
      var temp = oneLiteralRule(clauses)
      while (temp != None)
        temp = oneLiteralRule(temp.get)
      var newClauses = if (temp == None) clauses else temp.get
      newClauses = affirmativeNegatigeRule(newClauses)
      dpRec(resolutionRule(newClauses))
    }
  }

  /**
   * The one literal rule
   * @param clauses a list of clauses
   * @return a list of clauses with one unit literal eliminated
   */
  private def oneLiteralRule(clauses: List[Clause]): Option[List[Clause]] = {
    val unitClause = clauses.find(_.size == 1)
    if (unitClause == None)
      None
    else {
      val unitLit = unitClause.get.literals.head
      val newClauses = clauses.filterNot(_.contains(unitLit)) // delete clauses with the unitLit
      Some(newClauses.map(l => l.delete(unitLit.negate).asInstanceOf[Clause]))
    }
  }

  /**
   * The affirmative negative rule
   * @param clauses a list of clauses
   * @return a list of clauses with pure literals eliminated
   */
  private def affirmativeNegatigeRule(clauses: List[Clause]): List[Clause] = {
    val pos = clauses.map(_.consequence).flatten
    val neg = clauses.map(_.premise).flatten
    val pure = neg.map(_.negate).filterNot(pos.contains).map(_.negate).union(pos.filterNot(neg.map(_.negate).contains))
    clauses.filter(_.literals.intersect(pure).isEmpty)
  }

  /**
   * The resolution rule
   * @param clauses a list of clauses
   * @return a list of clauses where one variable is eliminated by resolution (the one with the smallest blowup)
   */
  private def resolutionRule(clauses: List[Clause]): List[Clause] = {
    val vars = clauses.foldLeft(List[PLLiteral]())((l, e) => l ++ e.literals).distinct
    if (vars.isEmpty)
      clauses
    else {
      val lit = vars.minBy(resolutionBlowup(_, clauses))
      resolveOn(lit, clauses)
    }
  }

  /**
   * Eliminate a variable by computing all resolvents
   * @param p a variable
   * @param clauses a list of clauses
   * @return the set of clauses with p eliminated
   */
  private def resolveOn(p: PLLiteral, clauses: List[Clause]): List[Clause] = {
    val (pos, notpos) = clauses.partition(_.contains(p))
    val (neg, other) = notpos.partition(_.contains(p.negate))
    val _pos = pos.map(_.delete(p))
    val _neg = neg.map(_.delete(p.negate))
    var resolvents =
      for {
        c1 <- _pos
        c2 <- _neg
      } yield (c1 union c2).asInstanceOf[Clause]
    resolvents = resolvents.filterNot(_.isTautology)
    other ++ resolvents
  }

  /**
   * Compute the maximal blowup for all resolutions on a variable
   * @param p a variable
   * @param clauses a list of clauses
   * @return the maximal blowup
   */
  private def resolutionBlowup(p: PLLiteral, clauses: List[Clause]): Int = {
    val m = clauses.filter(_.contains(p)).size
    val n = clauses.filter(_.contains(p.negate)).size
    m * n - m - n
  }
}
