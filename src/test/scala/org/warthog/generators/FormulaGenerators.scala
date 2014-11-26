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

package org.warthog.generators

import org.warthog.generic.formulas._
import org.scalacheck.{Gen, Arbitrary}
import org.warthog.fol.formulas._
import org.warthog.generic.formulas.Implication

object FormulaGenerators {

  implicit def arbitraryFolFormula: Arbitrary[Formula[FOL]] =
    Arbitrary {
      val maxTermArity = 5
      val maxTermDepth = 5
      val maxPredArity = 5
      val maxNaryArity = 5
      val maxFormulaDepth = 5

      val suitableFunctorName = Gen.alphaStr map { s =>
        if (s.length>3)
          s.substring(0,3)
        else
          s
      }

      def genVariable: Gen[FOLVariable] = {
        for (name <- suitableFunctorName) yield
          FOLVariable(name)
      }

      def genFunction(termDepth: Int): Gen[FOLFunction] = {
        for {
          name <- suitableFunctorName
          arity <- Gen.choose(0, maxTermArity)
          terms <- Gen.listOfN(arity, genTerm(termDepth-1))
        } yield
          FOLFunction(name, terms: _*)
      }

      def genTerm(termDepth: Int): Gen[FOLTerm] = {
        if (termDepth == 0)
          genVariable
        else
          Gen.oneOf(genVariable, genFunction(termDepth))
      }

      def genPredicate(termDepth: Int): Gen[Formula[FOL]] =
        for {
          name <- suitableFunctorName
          arity <- Gen.choose(0, maxPredArity)
          terms <- Gen.listOfN(arity, genTerm(termDepth))
        } yield
          FOLPredicate(name, terms: _*)

      def genAtom(termDepth: Int): Gen[Formula[FOL]] = Gen.oneOf(
        genPredicate(termDepth), Gen.const(Verum()), Gen.const(Falsum())
      )

      def genNary(depth: Int): Gen[Formula[FOL]] =
        for {
          operator <- Gen.oneOf(Seq(And.apply[FOL] _, Or.apply[FOL] _))
          arity <- Gen.choose(2, maxNaryArity)
          operands <- Gen.listOfN(arity, genFormula(depth-1))
        } yield operator(operands)

      def genBinary(depth: Int): Gen[Formula[FOL]] =
        for {
          operator <- Gen.oneOf(Seq(Implication.apply[FOL] _))
          left <- genFormula(depth-1)
          right <- genFormula(depth-1)
        } yield operator(left, right)

      def genNot(depth: Int): Gen[Formula[FOL]] =
        for {
          operand <- genFormula(depth-1)
        } yield Not(operand)

      def genQuantifier(depth: Int): Gen[Formula[FOL]] =
        for {
          quantifier <- Gen.oneOf(Seq(
            FOLForAll.apply(_: FOLVariable, _: Formula[FOL]),
            FOLExists.apply(_: FOLVariable, _: Formula[FOL])))
          formula <- genFormula(depth-1)
          variable <- if (formula.freeVars.isEmpty) genVariable else Gen.oneOf(formula.freeVars.map(_.asInstanceOf[FOLVariable]))
        } yield quantifier(variable, formula)

      def genFormula(depth: Int): Gen[Formula[FOL]] =
        if (depth == 0)
          genAtom(maxTermDepth)
        else
          Gen.oneOf(genQuantifier(depth), genNary(depth), genBinary(depth), genNot(depth), genAtom(depth))

      Gen.sized { depth =>
        if (depth >= maxFormulaDepth)
          genFormula(maxFormulaDepth)
        else
          genFormula(depth)
      }
    }
}
