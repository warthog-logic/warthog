/*
 * Copyright (c) 2011-2014, Monika Kümmerle, Steffen Hildebrandt, Rouven Walter
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

package org.warthog.pl.decisionprocedures

import org.specs2.mutable.Specification
import java.io.File
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.MiniSatJavaCLI

class MiniSatJavaCLITest extends Specification {

  args(sequential = true)

  private def getFileString(file: String) =
    List("src", "test", "resources", "dimacs", file).mkString(File.separator)

  "Satisfiability of simple dimacs formulas" should {
    "be true for formula oneClauseFormula" in {
      MiniSatJavaCLI.solve(getFileString("oneClauseFormula.cnf")) must beTrue
    }
    "be false for formula oneEmptyClause" in {
      MiniSatJavaCLI.solve(getFileString("oneEmptyClause.cnf")) must beFalse
    }
    "be true for formula oneVariableFormula" in {
      MiniSatJavaCLI.solve(getFileString("oneVariableFormula.cnf")) must beTrue
    }
    "be true for formula f01" in {
      MiniSatJavaCLI.solve(getFileString("f01.cnf")) must beTrue
    }
    "be true for formula f02" in {
      MiniSatJavaCLI.solve(getFileString("f02.cnf")) must beTrue
    }
    "be false for formula f03" in {
      MiniSatJavaCLI.solve(getFileString("f03.cnf")) must beFalse
    }
    "be false for formula f04" in {
      MiniSatJavaCLI.solve(getFileString("f04.cnf")) must beFalse
    }
    "be false for formula f05" in {
      MiniSatJavaCLI.solve(getFileString("f05.cnf")) must beFalse
    }
    "be false for formula f06" in {
      MiniSatJavaCLI.solve(getFileString("f06.cnf")) must beFalse
    }
    "be false for formula f07" in {
      MiniSatJavaCLI.solve(getFileString("f07.cnf")) must beFalse
    }
    "be false for formula f08" in {
      MiniSatJavaCLI.solve(getFileString("f08.cnf")) must beFalse
    }
    "be false for formula f09" in {
      MiniSatJavaCLI.solve(getFileString("f09.cnf")) must beFalse
    }
    "be false for formula f10" in {
      MiniSatJavaCLI.solve(getFileString("f10.cnf")) must beFalse
    }
    "be false for formula f11" in {
      MiniSatJavaCLI.solve(getFileString("f11.cnf")) must beFalse
    }
  }

  "Satisfiability of harder dimacs formulas" should {
    "be true for formula uf150-010" in {
      MiniSatJavaCLI.solve(getFileString("uf150-010.cnf")) must beTrue
    }
    "be true for formula uf150-027" in {
      MiniSatJavaCLI.solve(getFileString("uf150-027.cnf")) must beTrue
    }
    "be false for formula uuf150-011" in {
      MiniSatJavaCLI.solve(getFileString("uuf150-011.cnf")) must beFalse
    }
    "be false for formula uuf150-024" in {
      MiniSatJavaCLI.solve(getFileString("uuf150-024.cnf")) must beFalse
    }
  }
}
