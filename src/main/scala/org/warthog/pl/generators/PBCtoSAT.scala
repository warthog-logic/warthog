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

package org.warthog.pl.generators

import org.warthog.pl.datastructures.cnf.{ PLLiteral => Lit, ImmutablePLClause => Clause }
import scala.collection.mutable.{ HashMap }

/**
 * CNF-encoding of pseudo-Boolean constraints
 *
 *   O. Bailleux, Y. Boufkhad, O. Roussel:
 *   "A Translation of Pseudo-Boolean Constraints to SAT",
 *   Proc. of CP 2006
 *
 * In the worst case, the size of the produced formula can be exponentially related to
 * the size of the input constraint, but Boolean cardinality constraints (and some
 * other classes) are encoded in polynomial time and size.
 *
 * Note: The list of weights (ws) has to be in ascending order (w1 <= w2 <= ...)
 */
class PBCtoSAT(ws: List[Int], b: Int) {

  private val weights: List[Int] = ws
  private val bound: Int = b

  private def isTerminal(i: Int, b: Int): Boolean = (b <= 0) || (weights.take(i).sum <= b)

  /**
   * Method is assuming there are no fixed literals! (=> all literals are free)
   *
   */
  def encode() = encodeWorker(ws.length, bound, new HashMap)

  private def encodeWorker(i: Int, b: Int, used: HashMap[String,Boolean]): List[Clause] =
    if (used.contains("D_"+i+","+b)) List()
    else if (!isTerminal(i, b)) {
      val dib:String = "D_" + i + "," + b
      val di1bw:String = "D_" + (i - 1) + "," + (b - weights(i-1))
      val di1b:String = "D_" + (i - 1) + "," + b
      val xi:String = "x_" + i
      val newElems = List(new Clause(Lit(di1bw, false), Lit(dib, true)),
        new Clause(Lit(dib, false), Lit(di1b, true)),
        new Clause(Lit(dib, false), Lit(xi, false), Lit(di1bw, true)),
        new Clause(Lit(di1b, false), Lit(xi, true), Lit(dib, true)))
      used += ((dib,true))
      newElems ::: encodeWorker(i - 1, b, used) ::: encodeWorker(i - 1, b - weights(i-1), used)
    } else if (b == 0) {
      val di0:String = "D_" + i + ",0"
      val xjs:List[String] = List.iterate(1,i)(s => s + 1).map(j => "x_" + j)
      new Clause(Lit(di0, true) :: xjs.map(j => Lit(j,true))) :: xjs.map(j => new Clause(Lit(di0, false), Lit(j,false)))
    } else if (b < 0) {
      val varName:String = "D_" + i + "," + b
      List(new Clause(List(Lit(varName, false))))
    } else { // when sum_j=1^i w_j <= b
      val varName:String = "D_"+i+","+b
      List(new Clause(List(Lit(varName,true))))
    }


}
