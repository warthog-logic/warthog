package fol.formulas

import org.warthog.generic.formulas.Formula
import org.warthog.fol.formulas.FOL
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import generators.FormulaGenerators._

class BasicFormulaFuzzing extends Specification with ScalaCheck {
  implicit val params = set(minTestsOk -> 200)

  "all FOL formulas" should {
    "be in PNF after converting them to PNF" in {
      check { (fm: Formula[FOL]) => fm.pnf.isPNF }
    }

    "be in NNF after converting them to NNF" in {
      check { (fm: Formula[FOL]) => fm.nnf.isNNF }
    }

    "have the same sets of free variables before and after PNF conversion" in {
      check { (fm: Formula[FOL]) => fm.freeVars == fm.pnf.freeVars }
    }

    "only introduce new functions uppon skolemization" in {
      check { (fm: Formula[FOL]) =>
        fm.functions.toSet subsetOf fm.skolemize.functions.toSet }
    }

    "not contain free variables after existential closure" in {
      check { (fm: Formula[FOL]) => fm.existentialClosure.freeVars.isEmpty }
    }

    "not contain free variables after universal closure" in {
      check { (fm: Formula[FOL]) => fm.universalClosure.freeVars.isEmpty }
    }

    "be invariant wrt to their set of predicates on skolemization" in {
      check { (fm: Formula[FOL]) => fm.predicates == fm.skolemize.predicates }
    }

    "be invariant wrt to their set of predicates on PNF conversion" in {
      check { (fm: Formula[FOL]) => fm.predicates == fm.pnf.predicates }
    }
  }
}
