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

package org.warthog.fol.formulas

import org.warthog.generic.formulas.Formula
import org.warthog.generators.FormulaGenerators._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.specs2.matcher.Parameters

class BasicFormulaFuzzing extends Specification with ScalaCheck {
  implicit val params = Parameters(minTestsOk = 200)

  "all FOL formulas" should {
    "be in PNF after converting them to PNF" !
      prop { (fm: Formula[FOL]) => fm.pnf.isPNF }

    "be in NNF after converting them to NNF" !
      prop { (fm: Formula[FOL]) => fm.nnf.isNNF }

    "have the same sets of free variables before and after PNF conversion" !
      prop { (fm: Formula[FOL]) => fm.freeVars == fm.pnf.freeVars }

    "only introduce new functions uppon skolemization" !
      prop { (fm: Formula[FOL]) => fm.functions.toSet subsetOf fm.skolemize.functions.toSet }

    "not contain free variables after existential closure" !
      prop { (fm: Formula[FOL]) => fm.existentialClosure.freeVars.isEmpty }

    "not contain free variables after universal closure" !
      prop { (fm: Formula[FOL]) => fm.universalClosure.freeVars.isEmpty }

    "be invariant wrt to their set of predicates on skolemization" !
      prop { (fm: Formula[FOL]) => fm.predicates == fm.skolemize.predicates }

    "be invariant wrt to their set of predicates on PNF conversion" !
      prop { (fm: Formula[FOL]) => fm.predicates == fm.pnf.predicates }
  }
}
