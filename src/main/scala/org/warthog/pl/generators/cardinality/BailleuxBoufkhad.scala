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

package org.warthog.pl.generators.cardinality

import org.warthog.pl.formulas.{PL, PLAtom}
import org.warthog.generic.formulas._

/**
 * Sorting net based approach taken from
 *
 *   O. Bailleux, Y. Boufkhad:
 *   "Efficient CNF Encoding of Boolean Cardinality Constraints",
 *   Proc. of CP 2003
 *
 * Encoding size is quadratic in the size of inputs
 *
 * Author: kuebler
 * Date:   25.01.12
 */
object BailleuxBoufkhad extends SortingBasedCC {
  /* TODO: Requires extensive testing */
  private def totalitarize(in0: Array[PLAtom], in1: Array[PLAtom], out: Array[PLAtom]) = {
    val (len0, len1) = (in0.length, in1.length)
    ((for {
      i <- 0 until len0
      j <- 0 until len1
      if i + j < len0 + len1
    } yield {
      And(
        /* make sure that if in0 is at least i, in1 at least j then the result is at least i+j */
        Implication(And(in0(i), in1(j)), out(i + j + 1)),
        /* make sure that if the result is i+j, then i0 is at least i or i1 is at least j */
        Implication(out(i + j), Or(in0(i), in1(j)))
      )
    }) ++
      (for (i <- 0 until len0) yield {
        And(
          /* if in0 is at least i, output is at least i as well */
          Implication(in0(i), out(i)),
          /* if output is at least len1+i, in0 is at least i */
          Implication(out(len1 + i), in0(i))
        )
      }) ++
      (for (i <- 0 until len1) yield {
        And(
          /* if in1 is at least i, output is at least i as well */
          Implication(in1(i), out(i)),
          /* if output is at least len0+i, in1 is at least i */
          Implication(out(len0 + i), in1(i))
        )
      })).reduceLeft(And(_, _))
  }

  def sorter(in: Array[PLAtom], pref: String): (Array[PLAtom], Formula[PL]) =
    if (in.length > 1) {
      val (lo, lf) = sorter(in.take(in.length / 2), pref + "l")
      val (ro, rf) = sorter(in.drop(in.length / 2), pref + "r")
      val out: Array[PLAtom] = (0 until in.length).map(s => PLAtom(pref + s)).toArray
      (out, And(totalitarize(lo, ro, out), lf, rf))
    } else {
      (in, Verum())
    }
}

/* Usage e.g.:
object TestBB {
  def main(args: Array[String]) {
    val in: Array[Atom] = (1 to 3).map(s => Variable("v_"+s)).toArray
    println("\\sum_{1<=i<=3} v_i <= 2: "+BailleuxBoufkhad.le(in, 2))
    println("\\sum_{1<=i<=3} v_i >= 2: "+BailleuxBoufkhad.ge(in, 2))
    println("\\sum_{1<=i<=3} v_i == 2: "+BailleuxBoufkhad.eq(in, 2))
    println("\\sum_{1<=i<=3} v_i <  2: "+BailleuxBoufkhad.lt(in, 2))
    println("\\sum_{1<=i<=3} v_i >  2: "+BailleuxBoufkhad.gt(in, 2))
  }
}
*/
