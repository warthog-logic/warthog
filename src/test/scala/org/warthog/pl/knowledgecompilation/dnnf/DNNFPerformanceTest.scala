/*
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

package org.warthog.pl.knowledgecompilation.dnnf

import org.warthog.generic.formulas.{Xor, Formula}
import org.warthog.pl.formulas.{PLFormula, PL}
import org.warthog.pl.parsers.tptp._
import org.warthog.pl.F
import org.specs2.mutable._
import org.specs2.specification._
import org.warthog.pl.decisionprocedures.satsolver.impl.picosat.Picosat
import org.warthog.pl.decisionprocedures.satsolver.{Infinity, Duration, sat}
import org.warthog.generic.parsers.DIMACSReader
import org.warthog.pl.knowledgecompilation.dnnf.DNNFCompilation._
import java.io.File
import org.specs2.control.IncludeExcludeStackTraceFilter

/**
 * Tests for DNNF-Compilation and Operations
 */

class DNNFPerformanceTest extends Specification {

  /**
   * Execute time expensive tests
   */
  val expensiveTests = true

  object MyStackTraceFilter extends IncludeExcludeStackTraceFilter(
    Seq(),
    Seq("org.specs2", "scalaz\\.", "scala\\.", "sbt\\.", "^java\\.", "com.intellij", "org.junit", "org.eclipse.jdt"))

  args.report(traceFilter = MyStackTraceFilter)

  args(sequential = true)

  val ps = new Picosat

  private def getFileString(file: String, kind: String) =
    List("src", "test", "resources", kind, file).mkString(File.separator)

  def checkEquality(formula: Formula[PL], dnnf: DNNF, ps: Picosat, duration: Long = -1): Int = {
    val checkFormula = new PLFormula(Xor(formula, dnnf.asPLFormula)).tseitinCNF
    var result = 0
    sat(ps) {
      solver => {
        solver.add(checkFormula)
        result = -solver.sat(if (duration > 0) new Duration(duration) else Infinity)
      }
    }
    result
  }

  def compileT(f: Formula[PL], dnnf: DNNF): Fragments =
    "Compilation of " + f should {
      ("return " + dnnf + " using the Simple Compiler") in {
        compile(Simple, f) structuralEquals dnnf
      }
      ("return " + dnnf + " using the Advanced Compiler") in {
        compile(Advanced, f) structuralEquals dnnf
      }
    }

  def picoCheck(f: Formula[PL]): Fragments =
    "Checking equality of " + f + " and its compiled DNNF using Picosat" should {
      "return true using the Simple Compiler" in {
        checkEquality(f, compile(Simple, f), ps) must be greaterThan 0
      }
      "return true using the Advanced Compiler" in {
        checkEquality(f, compile(Advanced, f), ps) must be greaterThan 0
      }
    }

  "Performance Test" should {
    "" in {
      println("!!! EXECUTING PERFORMANCE TEST !!!")
      compileWithC2DDTree(getFileString("F03-20120606.cnf", "automotiveFormulas"))
      0 must be equalTo 0
    }
  }

}
