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

package org.warthog.pl.generators

import org.warthog.generic.formulas._
import org.warthog.pl.formulas.PLAtom

/**
 * Produce a propositional encoding of the Langford pairing problem
 *
 * Author: kuebler
 * Date:   25.01.12
 */
object LangfordPairingGenerator {
  private def ge_one_position(n: Int) =
    And(
      (for (i <- 1 to 2 * n) yield
        Or((for (j <- 1 to 2 * n) yield PLAtom("v" + placeat(i, j, n))): _*)): _*)

  private def le_one_position(n: Int) =
    And((for {i <- 1 to 2 * n
              j <- 1 to 2 * n
              k <- 1 to 2 * n if j != k} yield
      Implication(PLAtom("v" + placeat(i, k, n)), -PLAtom("v" + placeat(i, j, n)))): _*)

  private def le_one_per_position(n: Int) =
    And((for {i <- 1 to 2 * n
              j <- 1 to 2 * n
              k <- 1 to 2 * n if j != k} yield
      Implication(PLAtom("v" + placeat(j, i, n)), -PLAtom("v" + placeat(k, i, n)))): _*)

  private def langford_prop(n: Int) =
    And((for {i <- 1 to n
              j <- 1 to 2 * n - i - 1} yield
      Implication(PLAtom("v" + placeat(fst(i), j, n)), PLAtom("v" + placeat(snd(i), j + i + 1, n)))): _*) &&
      And((for {i <- 1 to n
                j <- 2 * n - i to 2 * n} yield
        -PLAtom("v" + placeat(fst(i), j, n))): _*)

  /**first of pair with order n */
  private def fst(i: Int) = 2 * i - 1

  private def snd(i: Int) = 2 * i

  private def placeat(i: Int, j: Int, n: Int): Int = ((i - 1) * (2 * n) + (j - 1)) + 1

  def generate(n: Int) =
    ge_one_position(n) &&
      le_one_position(n) &&
      le_one_per_position(n) &&
      langford_prop(n)
}
