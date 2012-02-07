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
 * Bitonic sorting net (acc. to Cormen, Leiserson, Rivest, Stein: Introduction
 * to Algorithms, 2nd edition, ch. 27)
 *
 * Encoding complexity: n lg**2 n comparisons
 *
 * Author: kuebler
 * Date:   25.01.12
 */
object BitonicSorting extends SortingBasedCC with PowerOf2 {
  // TODO: Requires extensive testing
  private def comparator(in0: PLAtom, in1: PLAtom, out0: PLAtom, out1: PLAtom): (Array[PLAtom], Formula[PL]) = {
    (Array(out0, out1),
      And(
        Equiv(And(in0, in1), out1),
        Equiv(Or(in0, in1), out0)
      )
      )
  }

  private def halfcleaner(in: Array[PLAtom], auxpref: String) = {
    val dist = in.length / 2
    val out = ((0 until dist * 2).map(s => PLAtom(auxpref + s)) ++ (dist * 2 until in.length).map(in(_))).toArray
    (out, (0 until dist).map(i => comparator(in(i), in(i + dist), out(i), out(i + dist))._2)
      .foldLeft(Verum(): Formula[PL])(And(_, _)))
  }

  private def bitonicsorter(in: Array[PLAtom], auxpref: String): (Array[PLAtom], Formula[PL]) = {
    val (co, cf) = halfcleaner(in, auxpref)
    if (in.length > 2) {
      val ((lco, lcf), (hco, hcf)) = (bitonicsorter(co.take(in.length / 2), auxpref + "l"),
        bitonicsorter(co.drop(in.length / 2), auxpref + "r"))
      (lco ++ hco, And(cf, lcf, hcf))
    } else
      (co, cf)
  }

  private def halfmerger(in: Array[PLAtom], auxpref: String): (Array[PLAtom], Formula[PL]) = {
    def l(in: Array[PLAtom], i: Int) = in.length - i - 1
    val dist = in.length / 2
    val odd = in.length % 2
    val out = ((0 until dist).map(s => PLAtom(auxpref + s)) ++
      (dist until dist + odd).map(in(_)) ++
      (dist + odd until in.length).map(s => PLAtom(auxpref + s))).toArray
    (out, (0 until dist).map(i => comparator(in(i), in(l(in, i)), out(i), out(l(in, i)))._2)
      .foldLeft(Verum(): Formula[PL])(And(_, _)))
  }

  private def merger(in: Array[PLAtom], auxpref: String): (Array[PLAtom], Formula[PL]) = {
    val (co, cf) = halfmerger(in, auxpref)
    if (in.length > 2) {
      val ((lco, lcf), (hco, hcf)) = (bitonicsorter(co.take(in.length / 2), auxpref + "l"),
        bitonicsorter(co.drop(in.length / 2), auxpref + "r"))
      (lco ++ hco, And(cf, lcf, hcf))
    } else
      (co, cf)
  }

  override def sorter(in: Array[PLAtom], auxpref: String) = {
    val k = nextPowerOf2(in.length) - in.length
    val extIn = (0 until k).map(s => PLAtom(auxpref + "dummyIn" + s))
    val (out, fm) = recsorter(in ++ extIn, auxpref)
    (out.take(in.length),
      And(
        fm,
        extIn.foldLeft(Verum(): Formula[PL])((f: Formula[PL], a: Formula[PL]) => And(f, -a)),
        out.drop(in.length).foldLeft(Verum(): Formula[PL])((f: Formula[PL], a: Formula[PL]) => And(f, -a))
      ))
  }

  private def recsorter(in: Array[PLAtom], auxpref: String): (Array[PLAtom], Formula[PL]) = {
    if (in.length > 2) {
      val ((lco, lcf), (hco, hcf)) = (recsorter(in.take(in.length / 2), auxpref + "l"),
        recsorter(in.drop(in.length / 2), auxpref + "r"))
      val (co, cf) = merger(lco ++ hco, auxpref + "m")
      (co, And(cf, lcf, hcf))
    } else {
      merger(in, auxpref + "m")
    }
  }
}

/* Usage e.g.:
object TestBS {
  def main(args: Array[String]) {
    val in: Array[Atom] = (1 to 3).map(s => Variable("v_"+s)).toArray
    val in0: Array[Atom]= (1 to 4).map(s => Variable("v_"+s)).toArray
    println("\\sum_{1<=i<=3} v_i <= 2: "+BitonicSorting.le(in, 2))
    println("\\sum_{1<=i<=3} v_i >= 2: "+BitonicSorting.ge(in, 2))
    println("\\sum_{1<=i<=3} v_i == 2: "+BitonicSorting.eq(in, 2))
    println("\\sum_{1<=i<=4} v_i <  2: "+BitonicSorting.lt(in0, 2))
    println("\\sum_{1<=i<=3} v_i >  2: "+BitonicSorting.gt(in, 2))
  }
}
*/
