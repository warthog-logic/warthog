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

package org.warthog.generic.parsers

import java.io.File
import org.warthog.pl.parsers.tptp._
import org.specs2.mutable.Specification
import org.warthog.generic.formulas._
import org.warthog.pl.datastructures.cnf.{ImmutablePLClause => PLClause, PLLiteral}
import org.warthog.fol.datastructures.cnf.{ImmutableFOLClause => FOLClause, FOLLiteral}
import org.warthog.fol.formulas._
import org.warthog.fol.formulas.FOLVariable

/**
 * Tests for each DIMACS Parser output
 *  - dimacs2Formula
 *  - dimacs2PLClauses
 *  - qdimacs2Formula
 */
class DIMACSTest extends Specification {

  val (vFOL1, vFOL2, vFOL3, vFOL4) = (FOLVariable("1"), FOLVariable("2"),FOLVariable("3"),FOLVariable("4"))
  val (pFOL1, pFOL2, pFOL3, pFOL4) = (FOLPredicate("1"), FOLPredicate("2"), FOLPredicate("3"), FOLPredicate("4"))

  val (vPLL1, vPLLn1 , vPLL2, vPLLn2, vPLL3, vPLLn3, vPLL4, vPLLn4) =
    (PLLiteral("1", true), PLLiteral("1", false),
    PLLiteral("2", true), PLLiteral("2", false),
    PLLiteral("3", true), PLLiteral("3", false),
    PLLiteral("4", true), PLLiteral("4", false))
  val (vFOLP1, vFOLPn1 , vFOLP2, vFOLPn2, vFOLP3, vFOLPn3, vFOLP4, vFOLPn4) =
    (FOLLiteral(pFOL1, true), FOLLiteral(pFOL1, false),
    FOLLiteral(pFOL2, true), FOLLiteral(pFOL2, false),
    FOLLiteral(pFOL3, true), FOLLiteral(pFOL3, false),
    FOLLiteral(pFOL4, true), FOLLiteral(pFOL4, false))

  private def getFileString(folder: String, file: String) =
    List("src", "test", "resources", folder, file).mkString(File.separator)

  "oneClauseFormula.cnf" should {
    "be equal to (1 | ~2 | ~3) (dimacs2Formula)" in {
      DIMACSReader.dimacs2Formula(getFileString("dimacs","oneClauseFormula.cnf")) must be equalTo And("(1 | ~2 | ~3)".pl)
    }

    val fPLClause = List(new PLClause(vPLL1, vPLLn2, vPLLn3))

    "be equal to (1 | ~2 | ~3) (dimacs2PLClauses)" in {
      DIMACSReader.dimacs2PLClauses(getFileString("dimacs","oneClauseFormula.cnf")) must be equalTo fPLClause
    }

    "be equal to (1 | ~2 | ~3) (qdimacs2Formula)" in {
      DIMACSReader.qdimacs2Formula(getFileString("dimacs","oneClauseFormula.cnf")) must be equalTo And(Or(pFOL1, Not(pFOL2), Not(pFOL3)))
    }

    val fFOLClause = List(new FOLClause(vFOLP1, vFOLPn2, vFOLPn3))
    "be equal to (List(), (1 | ~2 | ~3)) (qdimacs2FOLClauses)" in {
      DIMACSReader.qdimacs2FOLClauses(getFileString("dimacs","oneClauseFormula.cnf")) must be equalTo (List(),fFOLClause)
    }
  }


  "oneEmptyClause.cnf" should {
    "be equal to () (dimacs2Formula)" in {
      DIMACSReader.dimacs2Formula(getFileString("dimacs","oneEmptyClause.cnf")) must be equalTo And(Or())
    }

    val fPLClause = List(new PLClause())

    "be equal to () (dimacs2PLClauses)" in {
      DIMACSReader.dimacs2PLClauses(getFileString("dimacs","oneEmptyClause.cnf")) must be equalTo fPLClause
    }

    "be equal to () (qdimacs2Formula)" in {
      DIMACSReader.qdimacs2Formula(getFileString("dimacs","oneEmptyClause.cnf")) must be equalTo And(Or())
    }

    val fFOLClause = List(new FOLClause())

    "be equal to (List(),()) (qdimacs2FOLClauses)" in {
      DIMACSReader.qdimacs2FOLClauses(getFileString("dimacs","oneEmptyClause.cnf")) must be equalTo (List(), fFOLClause)
    }
  }

  "oneVariableFormula.cnf" should { // should not be equal to f
    val f = "(1 | ~1) & (1 | ~1) & (~1)"
    "be equal to "+f+" (dimacs2Formula)" in {
      DIMACSReader.dimacs2Formula(getFileString("dimacs","oneVariableFormula.cnf")) must be equalTo And("1 | ~1".pl, "1 | ~1".pl, Or("~1".pl))
    }

    val fPLClause = List(new PLClause(vPLL1, vPLLn1), new PLClause(vPLL1, vPLLn1), new PLClause(vPLLn1))

    "be equal to "+f+" (dimacs2PLClauses)" in {
      DIMACSReader.dimacs2PLClauses(getFileString("dimacs","oneVariableFormula.cnf")) must be equalTo fPLClause
    }

    "be equal to "+f+" (qdimacs2Formula)" in {
      DIMACSReader.qdimacs2Formula(getFileString("dimacs","oneVariableFormula.cnf")) must be equalTo And(Or(pFOL1,Not(pFOL1)), Or(pFOL1, Not(pFOL1)), Or(Not(pFOL1)))
    }

    val fFOLClause = List(new FOLClause(vFOLP1, vFOLPn1), new FOLClause(vFOLP1, vFOLPn1), new FOLClause(vFOLPn1))
    "be equal to (List(),"+f+") (qdimacs2FOLClauses)" in {
      DIMACSReader.qdimacs2FOLClauses(getFileString("dimacs","oneVariableFormula.cnf")) must be equalTo (List(),fFOLClause)
    }
  }

  "f01.cnf" should {

    val f = "(1 | ~2) & (~1 | 2 | ~3) & (~3 | 2) & (1 | 3)"
    "be equal to " + f + " (dimacs2Formula)" in {
      DIMACSReader.dimacs2Formula(getFileString("dimacs", "f01.cnf")) must be equalTo f.pl
    }

    val fPLClause = List(new PLClause(vPLL1, vPLLn2), new PLClause(vPLLn1, vPLL2, vPLLn3),
      new PLClause(vPLLn3, vPLL2), new PLClause(vPLL1, vPLL3))

    "be equal to " + f + " (dimacs2PLClauses)" in {
      DIMACSReader.dimacs2PLClauses(getFileString("dimacs", "f01.cnf")) must be equalTo fPLClause
    }

    val fFOL = And(Or(pFOL1,Not(pFOL2)),Or(Not(pFOL1),pFOL2,Not(pFOL3)),Or(Not(pFOL3),pFOL2),Or(pFOL1,pFOL3)).asInstanceOf[Formula[FOL]]
    "be equal to "+fFOL+" (qdimacs2Formula)" in {
      DIMACSReader.qdimacs2Formula(getFileString("dimacs","f01.cnf")) must be equalTo fFOL
    }

    val fFOLClause = List(new FOLClause(vFOLP1, vFOLPn2), new FOLClause(vFOLPn1, vFOLP2, vFOLPn3),
      new FOLClause(vFOLPn3, vFOLP2), new FOLClause(vFOLP1, vFOLP3))
    "be equal to (List(),"+f+") (qdimacs2FOLClauses)" in {
      DIMACSReader.qdimacs2FOLClauses(getFileString("dimacs","f01.cnf")) must be equalTo (List(), fFOLClause)
    }
  }

  "f02.cnf" should { // -4 -1 2 -3 0
    val f = "(1 | ~2) & (~3 | ~4 | 2 | ~1) & (~3 | 2 | 4) & (2 | 1 | 3) & (2 | ~1 | ~4)"
    "be equal to "+f+" (dimacs2Formula)" in {
      DIMACSReader.dimacs2Formula(getFileString("dimacs","f02.cnf")) must be equalTo f.pl
    }

    val fPLClause = List(new PLClause(vPLL1, vPLLn2), new PLClause(vPLLn3, vPLLn4, vPLL2, vPLLn1), new PLClause(vPLLn3, vPLL2, vPLL4),
                         new PLClause(vPLL2, vPLL1, vPLL3), new PLClause(vPLL2, vPLLn1, vPLLn4))

    "be equal to "+f+" (dimacs2PLClauses)" in {
      DIMACSReader.dimacs2PLClauses(getFileString("dimacs","f02.cnf")) must be equalTo fPLClause
    }

  }

  "f03.cnf" should {
    val f = "(1 | ~2) & (~1 | 2 | ~3) & (~3 | 2) & (1 | 3) & (2 | 3) & (~3 | ~1) & (3 | ~1 | 2) & (~1 | ~2 | 3)"
    "be equal to "+f+" (dimacs2Formula)" in {
      DIMACSReader.dimacs2Formula(getFileString("dimacs","f03.cnf")) must be equalTo f.pl
    }

    val fPLClause = List(new PLClause(vPLL1, vPLLn2), new PLClause(vPLLn1, vPLL2, vPLLn3), new PLClause(vPLLn3, vPLL2), new PLClause(vPLL1, vPLL3),
                         new PLClause(vPLL2, vPLL3), new PLClause(vPLLn3,vPLLn1), new PLClause(vPLL3, vPLLn1, vPLL2), new PLClause(vPLLn1, vPLLn2, vPLL3))

    "be equal to "+f+" (dimacs2PLClauses)" in {
      DIMACSReader.dimacs2PLClauses(getFileString("dimacs","f03.cnf")) must be equalTo fPLClause
    }
  }

  "qdimacs/f01.cnf" should {

    val fFOL = FOLExists(Set(vFOL1,vFOL2,vFOL3,vFOL4), And(Or(Not(pFOL1),pFOL2),Or(pFOL2,Not(pFOL3),Not(pFOL4))).asInstanceOf[Formula[FOL]])
    val fStr = "?[1]: ?[2]: ?[3]: ?[4]: (~1 | 2) & (2 | ~3 | ~4)"
    "be equal to "+fStr+" (qdimacs2Formula)" in {
      DIMACSReader.qdimacs2Formula(getFileString("qdimacs","f01.cnf")) must be equalTo fFOL
    }

    val fFOLClause = List(new FOLClause(vFOLPn1, vFOLP2), new FOLClause(vFOLP2, vFOLPn3, vFOLPn4))
    "be equal to (List((?, Set(1,2,3,4))),"+fFOLClause+") (qdimacs2FOLClauses)" in {
      DIMACSReader.qdimacs2FOLClauses(getFileString("qdimacs","f01.cnf")) must be equalTo (List[(String, Set[Int])](("?", Set(1,2,3,4))), fFOLClause)
    }

  }

  "qdimacs/f02.cnf" should {

    val fFOL = FOLExists(Set(vFOL1, vFOL2), FOLForAll(vFOL3, FOLExists(vFOL4, And(Or(Not(pFOL1), pFOL2), Or(pFOL2, Not(pFOL3), Not(pFOL4))).asInstanceOf[Formula[FOL]])))
    val fStr = "?[1,2]: ![3]: ?[4]: (~1 | 2) & (2 | ~3 | ~4)"
    "be equal to "+fStr + " (qdimacs2Formula)" in {
      DIMACSReader.qdimacs2Formula(getFileString("qdimacs", "f02.cnf")) must be equalTo fFOL
    }

    val fFOLClause = List(new FOLClause(vFOLPn1, vFOLP2), new FOLClause(vFOLP2, vFOLPn3, vFOLPn4))
    "be equal to (List((?,Set(1,2)),(!,Set(3)),(?,Set(4))),"+fFOLClause+" (qdimacs2FOLClauses)" in {
      DIMACSReader.qdimacs2FOLClauses(getFileString("qdimacs","f02.cnf")) must be equalTo (List(("?", Set(1,2)),("!",Set(3)),("?",Set(4))), fFOLClause)
    }

  }

}
