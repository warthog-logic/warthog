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

package org.warthog.fol.formulas

import org.warthog.generic.formulas._
import org.warthog.generic.printer.PrettyPrinter
import org.warthog.fol.unification.Unification
import org.warthog.fol.transformations._

/**
 * Rich formula for first order logic
 *
 * Author: zengler, kuebler
 * Date:   18.01.12
 */
trait FOLTransformations extends Substitution with PNF with Matrix with Skolemization with QuantifierClosures

class FOLFormula(override val f: Formula[FOL]) extends FOLTransformations {

  /**
   * pretty print a formula
   * @param prettyPrinter the pretty printer to use
   * @return the pretty printed formula
   */
  def pp(implicit prettyPrinter: PrettyPrinter[FOL]) = prettyPrinter.print(f)

  /**
   * Get the list of function symbols in `f`
   * @return a list of function symbols
   */
  def functions: List[FunctionSymbol] = functionsl(f)

  /**
   * Get the list of predicate symbols in `f`
   * @return a list of predicate symbols
   */
  def predicates: List[PredicateSymbol] = predicatesl(f)

  private def functionsl(arg: Formula[FOL]): List[FunctionSymbol] = arg match {
    case Not(p)                 => functionsl(p)
    case q: BinaryOperator[FOL] => (functionsl(q.f1) union functionsl(q.f2)).distinct
    case NAryOperator(_, ps@_*) => (ps.foldLeft(List[FunctionSymbol]())((set, elem) => set union functionsl(elem))).distinct
    case q: Quantifier[FOL]     => functionsl(q.arg)
    case p: FOLPredicate        => p.functions
    case _                      => List[FunctionSymbol]()
  }

  private def predicatesl(arg: Formula[FOL]): List[PredicateSymbol] = arg match {
    case Not(p)                 => predicatesl(p)
    case q: BinaryOperator[FOL] => (predicatesl(q.f1) union predicatesl(q.f2)).distinct
    case NAryOperator(_, ps@_*) => (ps.foldLeft(List[PredicateSymbol]())((set, elem) => set union predicatesl(elem))).distinct
    case q: Quantifier[FOL]     => predicatesl(q.arg)
    case q: FOLPredicate        => List[PredicateSymbol](q.symbol)
    case _                      => List[PredicateSymbol]()
  }

  def unifiableWith: Formula[FOL] => Boolean = Unification.unifyable(f, _)

  def mgu: Formula[FOL] => Map[FOLVariable, FOLTerm] = Unification.unify(f, _)
}
