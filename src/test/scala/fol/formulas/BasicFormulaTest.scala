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

package fol.formulas

import org.specs2.mutable._
import fol.F
import org.warthog.fol.parsers.tptp._
import org.warthog.fol.formulas.{FunctionSymbol, PredicateSymbol, FOLVariable}

/**
 * Basic tests for FOL generic
 * + normal forms
 * + transformations & simplicifations
 *
 * Author: zengler
 * Date:   25.01.12
 */
class BasicFormulaTest extends Specification {
  val X = FOLVariable("X")
  val Y = FOLVariable("Y")
  val Z = FOLVariable("Z")
  val U = FOLVariable("U")
  val V = FOLVariable("V")

  "x" should {
    "be in NNF" in {
      F.x.fol.isNNF must be equalTo true
    }
    "have a NNF of x" in {
      F.x.fol.nnf must be equalTo F.x.fol
    }
    "be ground" in {
      F.x.fol.isGround must be equalTo true
    }
    "have an empty set of variables" in {
      F.x.fol.vars must have size 0
    }
    "have an empty set of free variables" in {
      F.x.fol.freeVars must have size 0
    }
    "have an empty set of bound variables" in {
      F.x.fol.boundVars must have size 0
    }
    "have an empty set of functions" in {
      F.x.fol.functions must have size 0
    }
    "have a set of predicate Symbols {p}" in {
      F.x.fol.predicates must be equalTo List(PredicateSymbol("x", 0))
    }
  }

  F.h144 should {
    val h144nnf = "(?[X]: ~p(X) & ~r(Y)) | ?[Y]: q(Y) | ![Z]: ~p(Z) | ~q(Z)"
    "not be in NNF" in {
      F.h144.fol.isNNF must be equalTo false
    }
    "have a NNF of " + h144nnf in {
      F.h144.fol.nnf must be equalTo h144nnf.fol
    }
    "not be ground" in {
      F.h144.fol.isGround must be equalTo false
    }
    "have a set of variables {X,Y,Z}" in {
      F.h144.fol.vars must be equalTo List(X, Y, Z)
    }
    "have a set of free variables {Y}" in {
      F.h144.fol.freeVars must be equalTo List(Y)
    }
    "have a set of bound variables {X,Y,Z}" in {
      F.h144.fol.boundVars must be equalTo List(X, Y, Z)
    }
    "have an empty set of functions" in {
      F.h144.fol.functions must have size 0
    }
    "have a set of predicates {p,q,r}" in {
      F.h144.fol.predicates must be equalTo List(PredicateSymbol("p", 1), PredicateSymbol("r", 1), PredicateSymbol("q", 1))
    }
  }

  F.h150_1 should {
    val h150_1nnf = "?[Y]: ~(X < Y) | ![U]: ?[V]: mul(X,U) < mul(Y,V)"
    "not be in NNF" in {
      F.h150_1.fol.isNNF must be equalTo false
    }
    "have a NNF of " + h150_1nnf in {
      F.h150_1.fol.nnf must be equalTo h150_1nnf.fol
    }
    "not be ground" in {
      F.h150_1.fol.isGround must be equalTo false
    }
    "have a set of variables {X,Y,U,V}" in {
      F.h150_1.fol.vars must be equalTo List(Y, X, U, V)
    }
    "have a set of free variables {X}" in {
      F.h150_1.fol.freeVars must be equalTo List(X)
    }
    "have a set of bound variables {Y,U,V}" in {
      F.h150_1.fol.boundVars must be equalTo List(Y, U, V)
    }
    "have a set of functions {mul}" in {
      F.h150_1.fol.functions must be equalTo List(FunctionSymbol("mul", 2))
    }
    "have a set of predicates {<}" in {
      F.h150_1.fol.predicates must be equalTo List(PredicateSymbol("<", 2))
    }
  }

  F.h150_2 should {
    val h150_2nnf = "![X]: ~p(X) | (?[Y]: q(Y)) | ![Z]: ~p(Z) | ~q(Z)"
    "not be in NNF" in {
      F.h150_2.fol.isNNF must be equalTo false
    }
    "have a NNF of " + h150_2nnf in {
      F.h150_2.fol.nnf must be equalTo h150_2nnf.fol
    }
    "be ground" in {
      F.h150_2.fol.isGround must be equalTo true
    }
    "have a set of variables {X,Y,Z}" in {
      F.h150_2.fol.vars must be equalTo List(X,Y,Z)
    }
    "have an empty set of free variables" in {
      F.h150_2.fol.freeVars must have size 0
    }
    "have a set of bound variables {X,Y,Z}" in {
      F.h150_2.fol.boundVars must be equalTo List(X,Y,Z)
    }
    "have an empty set of functions" in {
      F.h150_2.fol.functions must have size 0
    }
    "have a set of predicates {p,q}" in {
      F.h150_2.fol.predicates must be equalTo List(PredicateSymbol("p", 1), PredicateSymbol("q", 1))
    }
  }
}
