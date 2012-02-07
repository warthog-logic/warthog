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
 * Trait for sorting based cardinality constraints
 *
 * Author: kuebler
 * Date:   25.01.12
 */
trait SortingBasedCC {
  def sorter(in: Array[PLAtom], auxpref: String): (Array[PLAtom],Formula[PL])

  def apply(in: Array[PLAtom], lowerBound: Int, upperBound: Int, auxpref: String="out") = {
    require(lowerBound>=0, "lowerBound out of bounds")
    require(upperBound>=0, "upperBound out of bounds")
    require(lowerBound<=upperBound, "lowerBound>upperBound!")

    if (lowerBound>in.length) /* if lowerBound > in.length, the constraint is trivially unsat */
      Falsum()
    else if (lowerBound==0 && upperBound>=in.length) /* a sum of in.length booleans is always in [0,in.length] */
      Verum()
    else {
      val (o,f) = sorter(in, auxpref)
      And(
        f,
        //o.take(lowerBound).foldLeft(PropTrue: Formula)(And(_, _)),
        lowerB(o, lowerBound),
        //o.drop(upperBound).map(Negation(_)).foldLeft(PropTrue: Formula)(And(_,_))
        upperB(o, upperBound)
      )
    }
  }

  def le(in: Array[PLAtom], k: Int, auxpref: String="out") = apply(in, 0, k, auxpref)
  def ge(in: Array[PLAtom], k: Int, auxpref: String="out") = apply(in, k, in.length, auxpref)
  def eq(in: Array[PLAtom], k: Int, auxpref: String="out") = apply(in, k, k, auxpref)
  def lt(in: Array[PLAtom], k: Int, auxpref: String="out") = le(in, k-1, auxpref)
  def gt(in: Array[PLAtom], k: Int, auxpref: String="out") = ge(in, k+1, auxpref)
  def lowerB(out: Array[PLAtom], l: Int) = out.take(l).foldLeft(Verum(): Formula[PL])(And(_, _))
  def upperB(out: Array[PLAtom], u: Int) = out.drop(u).map(Not(_)).foldLeft(Verum(): Formula[PL])(And(_, _))
}
