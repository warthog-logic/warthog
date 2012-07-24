package generators

import org.warthog.generic.formulas._
import org.scalacheck.{Gen, Arbitrary}
import org.warthog.fol.formulas._
import org.warthog.fol.formulas.FOLVariable
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
        genPredicate(termDepth), Gen.value(Verum()), Gen.value(Falsum())
      )

      def genNary(depth: Int): Gen[Formula[FOL]] =
        for {
          operator <- Gen.oneOf(Seq(And.apply[FOL] _, Or.apply[FOL] _))
          arity <- Gen.choose(2, maxNaryArity)
          operands <- Gen.listOfN(arity, genFormula(depth-1))
        } yield operator(operands: _*)

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
