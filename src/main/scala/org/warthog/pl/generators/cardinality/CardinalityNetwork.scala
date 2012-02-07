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

import org.warthog.generic.formulas.{Formula, Or}
import org.warthog.pl.formulas.{PL, PLAtom}

/**
 * Cardinality Networks
 * Implementation follows Asin, Nieuwenhuis, Oliveras, Rodriguez-Carbonelle:
 * "Cardinality Networks: a theoretical and empirical study", Constraints,
 * 2011
 *
 * Note: Cardinality Networks (or other means of encoding cardinality constraints)
 *       are only relevant in settings where we would encounter x_1 + ... + x_n # k
 *       and k>1 (which isn't yet clear for the Audi code family settings)
 *
 * Author: kuebler
 * Date:   25.01.12
 */
class CardinalityNetwork(p: Int) {
  /* TODO: Untested code, requires some fixup (among the first lines of scala code I ever wrote ;)) */

  /**Cardinality constraints of form x_1 + ... + x_n <= p are first transformed to
   * x_1 + ... + x_{n+m} <= k (k: 2^r with 2^{r-1} <= p <= 2**r, m s.t. k divides (n+m)),
   * after that, the output variable c_p+1 is set to 0 which will result in all c_i with
   * p+1 < i <= n+m propagated to 0, the newly introduced x_{n+1}, ..., x_{n+m} are set to
   * 0 by introducing corresponding unit clauses or substituting and eliminating the
   * corresponding constant values
   */

  /**returns true if n is a power of 2 */
  def powerOf2(n: Int) = (n & (-n)) == n

  /**WARNING: bit twiddling function! Assumptions: bit width of Int: 32, n<=2^31-1
   * (n==2^31 would change sign of integer) */
  def nextPowerOf2(n: Int) = {
    if (n >= Int.MaxValue)
      throw new Exception("n (" + n + ") out of allowed range")
    var x = n - 1
    x |= x >> 1
    x |= x >> 2
    x |= x >> 4
    x |= x >> 8
    x |= x >> 16
    x + 1
  }

  /**Get least power of 2 >= p */
  def getK = if (powerOf2(p)) {
    p
  } else {
    nextPowerOf2(p)
  }

  /**Get least multiple of k >= n */
  def getM(n: Int) = if (n % getK == 0) {
    0
  } else {
    (((n / getK) + 1) * getK) - n
  }

  def alternating[T](b: Boolean, l: List[T]): List[T] = {
    (b, l) match {
      case (_, Nil)        => Nil
      case (true, h :: t)  => h :: alternating(false, t)
      case (false, h :: t) => alternating(true, t)
    }
  }

  /**Half Merging Network (cf. Asin, Nieuwenhuis et.al., pp. 4ff)
   *
   * Assumption: #l0==#l1, #l0 power of two
   */
  def hmerge_aux(l0: List[PLAtom], l1: List[PLAtom], aux: String): (List[PLAtom], List[Formula[PL]]) = {
    (l0, l1) match {
      case (a :: Nil, b :: Nil) => {
        /* (arg1 /\ arg2 => aux_2), (arg1 \/ arg2) => aux_1 */
        (List(PLAtom(aux + "_1"), PLAtom(aux + "_1")),
          List(Or(-a, -b, PLAtom(aux + "_2")),
            Or((-a), PLAtom(aux + "_1")),
            Or((-b), PLAtom(aux + "_1"))))
      }
      case (ll, lr)             => {
        /* (<d_1, ..., d_n>, S_odd) */
        val (odd_at, odd_f) = hmerge_aux(alternating(true, ll), alternating(true, lr), aux + "C")
        /* (<e_1, ..., e_n>, S_even) */
        val (eve_at, eve_f) = hmerge_aux(alternating(false, ll), alternating(false, lr), aux + "D")
        val Sprime = (for {i <- 1 to ll.length - 1}
        yield List(Or(-odd_at(i), -eve_at(i - 1), PLAtom(aux + "_" + (2 * i + 1))),
            Or(-odd_at(i), PLAtom(aux + "_" + (2 * i))),
            Or(-eve_at(i - 1), PLAtom(aux + "_" + (2 * i))))) reduceLeft (_ union _)
        /* r0 = <d_1, c_2, ..., c_n-1, e_n> */
        val r0 = (odd_at(0) :: (for {i <- (2 to (2 * ll.length - 1))} yield PLAtom(aux + "_" + i)).toList ++ List(eve_at(ll.length - 1)))
        /* (<d_1, c_2, ..., c_n-1, e_n>, S' u S_odd u S_even) */
        (r0, Sprime union odd_f union eve_f)
      }
    }
  }

  def hmerge(l0: List[PLAtom], l1: List[PLAtom]): (List[PLAtom], List[Formula[PL]]) = hmerge_aux(l0, l1, "C")

  /**Half Sorting Network (cf. Asin, Nieuwenhuis et.al., pp. ?)
   *
   * Assumption: #l0, #l0 power of two
   */
  def hsort_aux(l0: List[PLAtom], aux: String): (List[PLAtom], List[Formula[PL]]) = {
    if (l0.length == 2) {
      hmerge_aux(List(l0(0)), List(l0(1)), aux)
    } else {
      val (d, sd) = hsort_aux(l0.slice(0, l0.length / 2), aux + "D")
      val (dp, sdp) = hsort_aux(l0.slice(l0.length / 2, l0.length), aux + "E")
      val (r0, sm) = hmerge_aux(d, dp, aux)
      (r0, sd union sdp union sm)
    }
  }

  def hsort(l0: List[PLAtom]): (List[PLAtom], List[Formula[PL]]) = hsort_aux(l0, "C")

  /**Simplified Half-Merging Networks (cf. Asin, Nieuwenhuis et.al., pp. ?)
   *
   * Assumptions: Only interested in the n+1 first bits instead of all 2n
   */
  def smerge_aux(l0: List[PLAtom], l1: List[PLAtom], aux: String): (List[PLAtom], List[Formula[PL]]) = {
    (l0, l1) match {
      case (a :: Nil, b :: Nil) => {
        (List(PLAtom(aux + "_1"), PLAtom(aux + "_2")),
          List(Or(-a, -b, PLAtom(aux + "_2")),
            Or(-a, PLAtom(aux + "_1")),
            Or(-b, PLAtom(aux + "_1"))))
      }
      case (ll, lr)             => {
        val (odd_at, odd_f) = smerge_aux(alternating(true, ll), alternating(true, lr), aux + "C")
        val (eve_at, eve_f) = smerge_aux(alternating(false, ll), alternating(false, lr), aux + "D")
        val r0 = odd_at(0) :: (for {i <- 2 to ll.length + 1} yield PLAtom(aux + "_" + i)).toList
        val Sprime = (for {i <- 1 to ll.length / 2}
        yield List(Or(-odd_at(i), -eve_at(i - 1), PLAtom(aux + "_" + (2 * i + 1))),
            Or(-odd_at(i), PLAtom(aux + "_" + (2 * i))),
            Or(-eve_at(i - 1), PLAtom(aux + "_" + (2 * i))))).reduceLeft(_ union _)
        (r0, odd_f union eve_f union Sprime)
      }
    }
  }

  def smerge(l0: List[PLAtom], l1: List[PLAtom]) = smerge_aux(l0, l1, "C")

  /**K-Cardinality Networks (cf. Asin, Nieuwenhuis et.al., pp. ?)
   *
   * Assumptions: k divides #input, k power of 2
   */
  def card_aux(l0: List[PLAtom], k: Int, aux: String): (List[PLAtom], List[Formula[PL]]) = {
    if (l0.length == k) {
      hsort_aux(l0, aux)
    } else {
      val (fst_at, fst_f) = card_aux(l0.slice(0, k), k, aux + "D")
      val (snd_at, snd_f) = card_aux(l0.slice(k, l0.length), k, aux + "E")
      val (trd_at, trd_f) = smerge_aux(fst_at, snd_at, aux)
      (trd_at.slice(0, k), fst_f union snd_f union trd_f)
    }
  }

  def <=(l0: List[PLAtom], dummy: String, aux: String) = {
    val k = getK
    val m = getM(l0.length)
    val dummies = (for {i <- 1 to m} yield PLAtom(dummy + "_" + i)).toList
    val (atoms, clauses) = card_aux(l0 ++ dummies, k, aux)
    /* add negative unit clauses for redundantly introduced variables, set exact cardinality */
    (atoms, clauses ++ (for {i <- 1 to m} yield (-PLAtom(dummy + "_" + i)))
      ++ (if (p < k) {
      List(-PLAtom(aux + "_" + (p + 1)))
    } else {
      Nil
    }))
  }
}

object cardinality {

  class CardinalityBuilder(name: String) {
    def is(l: List[PLAtom]): CardinalityBase = new CardinalityBase(name, l)
  }

  class CardinalityBase(name: String, list: List[PLAtom]) {
    def le(p: Int) = (new CardinalityNetwork(p)) <=(list, "dummy_" + name, "aux_" + name)

    def lt(p: Int) = (new CardinalityNetwork(p - 1)) <=(list, "dummy_" + name, "aux_" + name)
  }

  implicit def toFoo(name: String) = new CardinalityBuilder(name)

  def apply(f: => (List[PLAtom], List[Formula[PL]])) = f
}
