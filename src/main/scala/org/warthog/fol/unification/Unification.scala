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

package org.warthog.fol.unification

import org.warthog.fol.formulas._
import org.warthog.generic.formulas.{ Formula, Not }

/**
  * First-order unification (taken from Baader, Nipkow: Term-Rewriting and All That)
  */
object Unification {

  private class UnunifiableException extends Exception("not unifiable")

  private def occurs(v: FOLVariable, t: FOLTerm): Boolean = t match {
    case FOLVariable(name)        => v == t
    case FOLFunction(sym, tms@_*) => tms exists { occurs(v, _) }
  }

  private def elim(x: FOLVariable, t: FOLTerm, eqs: List[(FOLTerm, FOLTerm)],
                   sub: Map[FOLVariable, FOLTerm]): Map[FOLVariable, FOLTerm] =
    if (occurs(x, t))
      throw new UnunifiableException
    else
      unify(eqs.map(x => (x._1.tsubst(sub), x._2.tsubst(sub))), sub)

  def unify(eqs: List[(FOLTerm, FOLTerm)], sub: Map[FOLVariable, FOLTerm]): Map[FOLVariable, FOLTerm] =
    eqs match {
      case Nil => sub
      case (v@FOLVariable(x), t) :: rest =>
        if (v == t)
          unify(rest, sub)
        else
          elim(v, t, rest, sub.map(x => (x._1, x._2.tsubst(Map(v -> t)))) + (v -> t))
      case (t, v@FOLVariable(x)) :: rest =>
        elim(v, t, rest, sub.map(x => (x._1, x._2.tsubst(Map(v -> t)))) + (v -> t))
      case (FOLFunction(f, fargs@_*), FOLFunction(g, gargs@_*)) :: rest =>
        if (f == g && fargs.length == gargs.length)
          unify(fargs.zip(gargs).toList ::: rest, sub)
        else
          throw new UnunifiableException
      case _ => throw new UnunifiableException
    }

  def unify(tm0: FOLTerm, tm1: FOLTerm): Map[FOLVariable, FOLTerm] =
    unify(List((tm0, tm1)), Map.empty[FOLVariable, FOLTerm])

  def unify(lit0: Formula[FOL], lit1: Formula[FOL]): Map[FOLVariable, FOLTerm] =
    (lit0.nnf, lit1.nnf) match {
      case (Not(FOLPredicate(a, aas@_*)), Not(FOLPredicate(b, bargs@_*))) if (a == b && aas.length == bargs.length) =>
        unify(aas.zip(bargs).toList, Map.empty[FOLVariable, FOLTerm])
      case (FOLPredicate(a, aas@_*), FOLPredicate(b, bargs@_*)) if (a == b && aas.length == bargs.length) =>
        unify(aas.zip(bargs).toList, Map.empty[FOLVariable, FOLTerm])
      case _ =>
        throw new UnunifiableException
    }

  private def unifyable(eqs: List[(FOLTerm, FOLTerm)]): Boolean =
    try {
      unify(eqs, Map.empty[FOLVariable, FOLTerm])
      true
    } catch {
      case _: Throwable => false
    }

  def unifyable(lit0: Formula[FOL], lit1: Formula[FOL]): Boolean = {
    def check(a: PredicateSymbol, b: PredicateSymbol, as: Seq[FOLTerm], bs: Seq[FOLTerm]) =
      if (a == b && as.length == bs.length)
        unifyable(as.zip(bs).toList)
      else
        false

    (lit0.nnf, lit1.nnf) match {
      case (Not(FOLPredicate(a, aargs@_*)), Not(FOLPredicate(b, bargs@_*))) =>
        check(a, b, aargs, bargs)

      case (FOLPredicate(a, aargs@_*), FOLPredicate(b, bargs@_*)) =>
        check(a, b, aargs, bargs)

      case _ => false
    }
  }

  def unifyable(tm0: FOLTerm, tm1: FOLTerm): Boolean = unifyable(List((tm0, tm1)))

}
