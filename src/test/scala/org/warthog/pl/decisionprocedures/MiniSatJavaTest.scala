package org.warthog.pl.decisionprocedures

import org.specs2.mutable.Specification
import java.io.File
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.MiniSatJava

class MiniSatJavaTest extends Specification {

  args(sequential = true)

  private def getFileString(file: String) =
    List("src", "test", "resources", "dimacs", file).mkString(File.separator)

  "Satisfiability of simple dimacs formulas" should {
    "be true for formula oneClauseFormula" in {
      MiniSatJava.solve(getFileString("oneClauseFormula.cnf")) must beTrue
    }
    "be false for formula oneEmptyClause" in {
      MiniSatJava.solve(getFileString("oneEmptyClause.cnf")) must beFalse
    }
    "be true for formula oneVariableFormula" in {
      MiniSatJava.solve(getFileString("oneVariableFormula.cnf")) must beTrue
    }
    "be true for formula f01" in {
      MiniSatJava.solve(getFileString("f01.cnf")) must beTrue
    }
    "be true for formula f02" in {
      MiniSatJava.solve(getFileString("f02.cnf")) must beTrue
    }
    "be false for formula f03" in {
      MiniSatJava.solve(getFileString("f03.cnf")) must beFalse
    }
    "be false for formula f04" in {
      MiniSatJava.solve(getFileString("f04.cnf")) must beFalse
    }
    "be false for formula f05" in {
      MiniSatJava.solve(getFileString("f05.cnf")) must beFalse
    }
    "be false for formula f06" in {
      MiniSatJava.solve(getFileString("f06.cnf")) must beFalse
    }
    "be false for formula f07" in {
      MiniSatJava.solve(getFileString("f07.cnf")) must beFalse
    }
    "be false for formula f08" in {
      MiniSatJava.solve(getFileString("f08.cnf")) must beFalse
    }
    "be false for formula f09" in {
      MiniSatJava.solve(getFileString("f09.cnf")) must beFalse
    }
    "be false for formula f10" in {
      MiniSatJava.solve(getFileString("f10.cnf")) must beFalse
    }
    "be false for formula f11" in {
      MiniSatJava.solve(getFileString("f11.cnf")) must beFalse
    }
  }

  "Satisfiability of harder dimacs formulas" should {
    "be true for formula uf150-010" in {
      MiniSatJava.solve(getFileString("uf150-010.cnf")) must beTrue
    }
    "be true for formula uf150-027" in {
      MiniSatJava.solve(getFileString("uf150-027.cnf")) must beTrue
    }
    "be false for formula uuf150-011" in {
      MiniSatJava.solve(getFileString("uuf150-011.cnf")) must beFalse
    }
    "be false for formula uuf150-024" in {
      MiniSatJava.solve(getFileString("uuf150-024.cnf")) must beFalse
    }
  }
}
