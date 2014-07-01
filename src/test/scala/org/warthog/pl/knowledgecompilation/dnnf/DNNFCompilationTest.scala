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
import org.warthog.pl.decisionprocedures.satsolver.{Solver, Infinity, Duration, sat}
import org.warthog.generic.parsers.DIMACSReader
import org.warthog.pl.knowledgecompilation.dnnf.DNNFCompilation._
import java.io.File
import org.specs2.control.IncludeExcludeStackTraceFilter
import org.warthog.pl.decisionprocedures.satsolver.impl.{MinisatJavaSolver, PicosatSolver}

/**
 * Tests for DNNF-Compilation and Operations
 */

class DNNFCompilationTest extends Specification {

  /**
   * Execute time expensive tests
   */
  val expensiveTests = false

  object MyStackTraceFilter extends IncludeExcludeStackTraceFilter(
    Seq(),
    Seq("org.specs2", "scalaz\\.", "scala\\.", "sbt\\.", "^java\\.", "com.intellij", "org.junit", "org.eclipse.jdt"))

  args.report(traceFilter = MyStackTraceFilter)

  args(sequential = true)

  val verifier = new MinisatJavaSolver

  private def getFileString(file: String, kind: String) =
    List("src", "test", "resources", kind, file).mkString(File.separator)

  def checkEquality(formula: Formula[PL], dnnf: DNNF, solver: Solver, duration: Long = -1): Int = {
    val checkFormula = new PLFormula(Xor(formula, dnnf.asPLFormula)).tseitinCNF
    var result = 0
    sat(solver) {
      s => {
        s.add(checkFormula)
        result = -s.sat(if (duration > 0) new Duration(duration) else Infinity)
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
        checkEquality(f, compile(Simple, f), verifier) must be greaterThan 0
      }
      "return true using the Advanced Compiler" in {
        checkEquality(f, compile(Advanced, f), verifier) must be greaterThan 0
      }
    }

  compileT(F.x.pl, F.x.dnnf)
  compileT(F.notx.pl, F.notx.dnnf)
  compileT(F.xy.pl, F.xy.dnnf)
  compileT(F.xoy.pl, F.xoy.dnnf)

  picoCheck(F.verum.pl)
  picoCheck(F.falsum.pl)
  picoCheck(F.x.pl)
  picoCheck(F.notx.pl)
  picoCheck(F.xy.pl)
  picoCheck(F.xoy.pl)
  picoCheck(F.n_xynz.pl)
  picoCheck(F.n_nxoyoz.pl)
  picoCheck(F.equiv1.pl)
  picoCheck(F.impl1_br.pl)
  picoCheck(F.impl1vv.pl)
  picoCheck(F.nxoyoz.pl)
  picoCheck(F.xyoz_br.pl)
  picoCheck(F.xorequiv1_br.pl)

  "Model count" should {
    "be 1511104 for formula uf150-010.cnf" in {
      val formula = DIMACSReader.dimacs2Formula(getFileString("uf150-010.cnf", "dimacs"))
      val vars = formula.vars.size
      DNNF.countModels(compile(Advanced, formula), vars) must be equalTo BigInt("1511104")
    }
    "be 67584 for formula uf150-027.cnf" in {
      val formula = DIMACSReader.dimacs2Formula(getFileString("uf150-027.cnf", "dimacs"))
      val vars = formula.vars.size
      val dnnf = compile(Advanced, formula)
      DNNF.countModels(dnnf, vars) must be equalTo BigInt("67584")
    }
  }

  if (expensiveTests && (new File(getFileString("", "automotiveFormulas")) exists())) {
    "Model count" should {
      "be 30401807433546007798154659399137233759263265705164800 for formula bmw_00004_sat.cnf" in {
        val vars = DIMACSReader.dimacs2Formula(getFileString("bmw_00004_sat.cnf", "automotiveFormulas")).vars.size
        val dnnf = compileWithC2DDTree(getFileString("bmw_00004_sat.cnf", "automotiveFormulas"))
        DNNF.countModels(dnnf, vars) must be equalTo BigInt("30401807433546007798154659399137233759263265705164800")
      }
      "be 0 for formula bmw_00052_unsat.cnf" in {
        val vars = DIMACSReader.dimacs2Formula(getFileString("bmw_00052_unsat.cnf", "automotiveFormulas")).vars.size
        DNNF.countModels(compileWithC2DDTree(getFileString("bmw_00052_unsat.cnf", "automotiveFormulas")), vars) must
          be equalTo BigInt("0")
      }
    }
  }
}
