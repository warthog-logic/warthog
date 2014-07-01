package org.warthog.pl.decisionprocedures

import org.specs2.mutable.Specification
import java.io.File
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.MinisatJava

class MiniSatJavaTest extends Specification {

  args(sequential = true)

  private def getFileString(file: String) =
    List("src", "test", "resources", "dimacs", file).mkString(File.separator)

  "Satisfiability of simple dimacs formulas" should {
    "be true for formula oneClauseFormula" in {
      MinisatJava.solve(getFileString("oneClauseFormula.cnf")) must beTrue
    }
    "be false for formula oneEmptyClause" in {
      MinisatJava.solve(getFileString("oneEmptyClause.cnf")) must beFalse
    }
    "be true for formula oneVariableFormula" in {
      MinisatJava.solve(getFileString("oneVariableFormula.cnf")) must beTrue
    }
    "be true for formula f01" in {
      MinisatJava.solve(getFileString("f01.cnf")) must beTrue
    }
    "be true for formula f02" in {
      MinisatJava.solve(getFileString("f02.cnf")) must beTrue
    }
    "be false for formula f03" in {
      MinisatJava.solve(getFileString("f03.cnf")) must beFalse
    }
    "be false for formula f04" in {
      MinisatJava.solve(getFileString("f04.cnf")) must beFalse
    }
    "be false for formula f05" in {
      MinisatJava.solve(getFileString("f05.cnf")) must beFalse
    }
    "be false for formula f06" in {
      MinisatJava.solve(getFileString("f06.cnf")) must beFalse
    }
    "be false for formula f07" in {
      MinisatJava.solve(getFileString("f07.cnf")) must beFalse
    }
    "be false for formula f08" in {
      MinisatJava.solve(getFileString("f08.cnf")) must beFalse
    }
    "be false for formula f09" in {
      MinisatJava.solve(getFileString("f09.cnf")) must beFalse
    }
    "be false for formula f10" in {
      MinisatJava.solve(getFileString("f10.cnf")) must beFalse
    }
    "be false for formula f11" in {
      MinisatJava.solve(getFileString("f11.cnf")) must beFalse
    }
  }

  "Satisfiability of harder dimacs formulas" should {
    "be true for formula uf150-010" in {
      MinisatJava.solve(getFileString("uf150-010.cnf")) must beTrue
    }
    "be true for formula uf150-027" in {
      MinisatJava.solve(getFileString("uf150-027.cnf")) must beTrue
    }
    "be false for formula uuf150-011" in {
      MinisatJava.solve(getFileString("uuf150-011.cnf")) must beFalse
    }
    "be false for formula uuf150-024" in {
      MinisatJava.solve(getFileString("uuf150-024.cnf")) must beFalse
    }
  }
}
