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

import scala.collection.mutable.HashMap

/**
 * CNF-encoding of pseudo-Boolean constraints
 *
 * O. Bailleux, Y. Boufkhad, O. Roussel:
 * "A Translation of Pseudo-Boolean Constraints to SAT",
 * Proc. of CP 2006
 *
 * In the worst case, the size of the produced formula can be exponentially related to
 * the size of the input constraint, but Boolean cardinality constraints (and some
 * other classes) are encoded in polynomial time and size.
 *
 * Note: All weights and the bound has to be greater than null
 */
object BailleuxBoufkhadRoussel extends PBCtoSAT {

  override def le(weights: List[Tuple2[Int,Lit]], bound: Int, prefix: String = "D_"): List[Clause] = {
    new BailleuxBoufkhadRousselHelper(weights, bound, prefix).encode()
  }
}

private class BailleuxBoufkhadRousselHelper(ws: List[Tuple2[Int,Lit]], b: Int, pre: String) extends PBCtoSAT {

  private val weights: List[Tuple2[Int,Lit]] = ws.sortBy(_._1)
  private val bound: Int = b
  private val prefix: String = pre

  private def isTerminal(i: Int, b: Int): Boolean = (b <= 0) || (weights.take(i).foldRight(0)((pair,r) => pair._1 + r) <= b)

  private def mkName(i: Int, b: Int): String = prefix + i + "_" + b

  /**
   * Method is assuming there are no fixed literals! (=> all literals are free)
   *
   */
  def encode() = new Clause(Lit(mkName(weights.length,bound),true)) :: encodeWorker(weights.length, bound, new HashMap)

  private def encodeWorker(i: Int, b: Int, used: HashMap[String, Boolean]): List[Clause] =
    if (used.contains(mkName(i, b))) List()
    else if (!isTerminal(i, b)) {
      val dib = mkName(i, b)
      val di1bw = mkName(i - 1, b - weights(i - 1)._1)
      val di1b = mkName(i - 1, b)
      val xi = weights(i-1)._2
      val newElems = List(new Clause(Lit(di1bw, false), Lit(dib, true)),
        new Clause(Lit(dib, false), Lit(di1b, true)),
        new Clause(Lit(dib, false), xi.negate, Lit(di1bw, true)),
        new Clause(Lit(di1b, false), xi, Lit(dib, true)))
      used += ((dib, true))
      newElems ::: encodeWorker(i - 1, b, used) ::: encodeWorker(i - 1, b - weights(i - 1)._1, used)
    } else if (b == 0) {
      val di0 = mkName(i, 0)
      val xjs = weights.take(i).map(pair => pair._2)
      new Clause(Lit(di0, true) :: xjs) :: xjs.map(xj => new Clause(Lit(di0, false), xj.negate))
    } else if (b < 0) {
      List(new Clause(Lit(mkName(i, b), false)))
    } else {
      // when sum_j=1^i w_j <= b
      List(new Clause(Lit(mkName(i, b), true)))
    }
}