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
import org.warthog.pl.formulas.{PLAtom, PL}

/**
 * Generator for pigeonhole problems
 *
 * Author: zengler
 * Date:   25.01.12
 */
object PigeonHoleGenerator {

  /**
   * Generate a pigeon hole instance for 'n+1' pigeons, n holes
   * @param n the number of pigeons
   * @return a pigeon hole instance for 'n+1' pigeons
   */
  def generate(n: Int): Formula[PL] = {
    require(n > 0, "Pigeon hole instance must have at least 1 pigeon")
    placeInSomeHole(n) && onlyOnePigeonInHole(n)
  }

  /* v_i,j: pigeon i sits in hole j */
  private def placeInSomeHole(n: Int): Formula[PL] = n match {
    case 1 => And(PLAtom("v1"), PLAtom("v2"))
    case _ => And((for {
      i <- 1 to n + 1
      val or = Or((for {
        j <- 1 to n
        val v = PLAtom("v" + (n * (i - 1) + j))
      } yield v): _*)
    } yield or): _*)
  }

  private def onlyOnePigeonInHole(n: Int): Formula[PL] = n match {
    case 1 => Or(-PLAtom("v1"), -PLAtom("v2"))
    case _ => And((for {
      j <- 1 to n
      i <- 1 to n
      k <- i + 1 to n + 1
      val or = -PLAtom("v" + (n * (i - 1) + j)) || -PLAtom("v" + (n * (k - 1) + j))
    } yield or): _*)
  }
}
