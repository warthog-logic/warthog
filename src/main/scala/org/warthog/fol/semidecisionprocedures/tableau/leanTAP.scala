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

package org.warthog.fol.semidecisionprocedures.tableau

import org.warthog.fol.formulas.{FOLTerm, FOLForAll, FOLVariable, FOL}
import org.warthog.fol.unification.Unification
import org.warthog.generic.formulas.{Verum, Or, And, Formula}

/**
 * leanTAP free variable tableau prover - adapted mostly from J. Harrison,
 * Handbook of Practical Logic and Automated Reasoning
 *
 * Author: kuebler
 * Date:   09.05.12
 */
object leanTAP {

  private def prove(fm: Formula[FOL], unexp: List[Formula[FOL]], lits: List[Formula[FOL]],
                      free: List[FOLVariable], bound: Int): Boolean =
      fm match {
        case And(opnds @ _*) =>
          prove(opnds.head,
            (if (opnds.tail.size > 1) And(opnds.tail: _*) else opnds.tail.head) :: unexp,
            lits, free, bound)

        case Or(opnds @ _*) =>
          /*
           * This one makes prove non-tail recursive! Might be avoided using continuations
           * (cf. Harrison, Handbook of Practical Logic and Automated Reasoning)
           */
          opnds.forall(prove(_, unexp, lits, free, bound))

        case FOLForAll(x, p) =>
          if (free.length < bound) {
            val xPrime = x.freshVariable(Verum[FOL], free.toSet)
            val pPrime = p.substitute(Map[FOLVariable, FOLTerm](x -> xPrime))

            prove(pPrime, unexp :+ fm, lits, xPrime :: free, bound)
          } else
            false

        case lit =>
          if (lits.exists(Unification.unifyable(-lit, _))) /* close branch */
            true
          else
            unexp match {
              case Nil =>
                throw new Exception("No proof! (SAT?)")

              case head :: tail =>
                prove(head, tail, lit :: lits, free, bound)
            }
      }

    def leanTAP(fm: Formula[FOL], bound: Int) = {
      /* transform formula to skolem normalform (nnf), compute universal closure */
      val fmPrime = fm.skolemize.universalClosure

      prove(fmPrime, Nil, Nil, Nil, bound)
    }
}
