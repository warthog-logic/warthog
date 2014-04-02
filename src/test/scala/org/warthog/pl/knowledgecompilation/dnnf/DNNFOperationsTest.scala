package org.warthog.pl.knowledgecompilation.dnnf

import org.specs2.mutable._
import org.warthog.generic.parsers.DIMACSReader
import java.io.File
import scala.collection.JavaConverters._
import org.specs2.control.IncludeExcludeStackTraceFilter

/**
 * Idea: Use DNNFOperations as if it were a SATSolver to test its behavior.
 */
class DNNFOperationsTest extends Specification {

  object MyStackTraceFilter extends IncludeExcludeStackTraceFilter(
    Seq(),
    Seq("org.specs2", "scalaz\\.", "scala\\.", "sbt\\.", "^java\\.", "com.intellij", "org.junit", "org.eclipse.jdt"))

  args.report(traceFilter = MyStackTraceFilter)

  args(sequential = true)

  private def getFileString(file: String) =
    List("src", "test", "resources", "dimacs", file).mkString(File.separator)

  private def solve(file: String): Boolean = {
    val clauses = DIMACSReader.dimacs2Clauses(getFileString(file))
    // cf. DIMACS handling in org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.MiniSatJava
    val solverClauses = clauses.map(_.map(i => if (i < 0) (((i * -1) - 1) * 2) ^ 1 else (i - 1) * 2))
    val javaClauses = solverClauses.map(_.map(new Integer(_)).asJava).asJava

    val solver = new DNNFOperations()
    solver.initSolver(javaClauses)
    solver.recursiveSolve()
  }

  "Satisfiability of simple dimacs formulas" should {
    "be true for formula f01" in {
      solve("f01.cnf") must beTrue
    }
    "be true for formula f02" in {
      solve("f02.cnf") must beTrue
    }
    "be false for formula f03" in {
      solve("f03.cnf") must beFalse
    }
    "be false for formula f04" in {
      solve("f04.cnf") must beFalse
    }
    "be false for formula f05" in {
      solve("f05.cnf") must beFalse
    }
  }

  "Satisfiability of harder dimacs formulas" should {
    "be true for formula uf150-010" in {
      solve("uf150-010.cnf") must beTrue
    }
    "be true for formula uf150-027" in {
      solve("uf150-027.cnf") must beTrue
    }
    "be false for formula uuf150-011" in {
      solve("uuf150-011.cnf") must beFalse
    }
    "be false for formula uuf150-024" in {
      solve("uuf150-024.cnf") must beFalse
    }
  }
}
