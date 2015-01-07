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

package org.warthog.pl.decisionprocedures

import satsolver.{Model, Solver, sat}
import org.specs2.mutable.Specification
import org.warthog.pl.formulas.{PL, PLAtom}
import org.warthog.generic.formulas.{Formula, Verum, Falsum}
import org.warthog.pl.decisionprocedures.satsolver.impl.picosat.Picosat
import java.io.File
import org.warthog.generic.parsers.DIMACSReader

/**
 * Tests for the picosat bindings
 */
class PicosatTest extends Specification {

  val (x, y, z) = (PLAtom("x"), PLAtom("y"), PLAtom("z"))
  val prover = new Picosat
  var resultValue0: Int = _
  var resultValue1: Int = _
  var model: Option[Model] = _

  /*
   * By default, tests are executed concurrently. JNI/JNA, however, is able to load _only one_ instance of
   * (lib)picosat.{so,dylib,dll} per JVM so concurrently accessing the picosat INSTANCE will result in double
   * instantiation errors and unexpected behaviour.
   */
  args(sequential = true)

  private def getFileString(folder: String, file: String) =
    List("src", "test", "resources", folder, file).mkString(File.separator)

  "x" should {
    "be satisfiable" in {
      sat(prover) {
        (solver: Solver) => {
          solver.add(x)
          resultValue0 = solver.sat()
        }
      }
      resultValue0 must be equalTo Solver.SAT
    }
    "be satisfied by model x" in {
      sat(prover) {
        (solver: Solver) => {
          solver.add(x)
          solver.sat()
          model = solver.getModel()
        }
      }
      model.get.positiveVariables.size must be equalTo 1
      model.get.negativeVariables.size must be equalTo 0
      model.get.positiveVariables must contain(x)
    }
    "be unsatisfiable after adding -x" in {
      sat(prover) {
        solver => {
          solver.add(x)
          solver.add(-x)
          resultValue0 = solver.sat()
        }
      }
      resultValue0 must be equalTo Solver.UNSAT
    }
    "be unsatisfiable after adding -x, satisfiable again after dropping -x" in {
      sat(prover) {
        solver => {
          solver.add(x)
          solver.mark()
          solver.add(-x)
          resultValue0 = solver.sat()
          solver.undo()
          resultValue1 = solver.sat()
        }
      }
      resultValue0 must be equalTo Solver.UNSAT
      resultValue1 must be equalTo Solver.SAT
    }
  }
  "the empty clause" should {
    "be satisfiable" in {
      sat(prover) {
        s => {
          s.add(Falsum())
          resultValue0 = s.sat()
        }
      }
      resultValue0 must be equalTo Solver.UNSAT
    }
  }
  "the empty formula" should {
    "be satisfiable" in {
      sat(prover) {
        s => {
          s.add(Verum())
          resultValue0 = s.sat()
        }
      }
      resultValue0 must be equalTo Solver.SAT
    }
  }
  "the verum" should {
    "return true upon sat checking" in {
      sat(prover) {
        s => {
          s.add(Verum())
          resultValue0 = s.sat()
          model = s.getModel()
        }
      }
      model.get.positiveVariables.size must be equalTo 0
      model.get.negativeVariables.size must be equalTo 0
    }
  }
  "x and -x" should {
    "be unsatisfiable even after multiple undo calls" in {
      sat(prover) {
        s => {
          s.add(x)
          s.add(-x)
          s.undo()
          s.undo()
          resultValue0 = s.sat()
        }
      }
      resultValue0 must be equalTo Solver.UNSAT
    }
  }

  private def testDIMACSFile(fileName: String, expResult: Int) {
    val expText = if (expResult == Solver.SAT) "satisfiable" else "unsatisfiable"
    "File " + fileName should {
      "be " + expText in {
        var resultVal = 0
        sat(prover) {
          (solver: Solver) => {
            prover.add(DIMACSReader.dimacs2PLClauses(getFileString("dimacs", fileName)))
            resultVal = solver.sat()
          }
        }
        resultVal must be equalTo expResult
      }
    }
  }

  testDIMACSFile("f01.cnf", Solver.SAT)
  testDIMACSFile("f02.cnf", Solver.SAT)
  testDIMACSFile("f03.cnf", Solver.UNSAT)
  testDIMACSFile("f04.cnf", Solver.UNSAT)

  testDIMACSFile("f05.cnf", Solver.UNSAT)
  testDIMACSFile("f06.cnf", Solver.UNSAT)
  testDIMACSFile("f07.cnf", Solver.UNSAT)
  testDIMACSFile("f08.cnf", Solver.UNSAT)

  testDIMACSFile("f09.cnf", Solver.UNSAT)
  testDIMACSFile("f10.cnf", Solver.UNSAT)
  testDIMACSFile("f11.cnf", Solver.UNSAT)

  testDIMACSFile("oneClauseFormula.cnf", Solver.SAT)
  testDIMACSFile("oneEmptyClause.cnf", Solver.UNSAT)
  testDIMACSFile("oneVariableFormula.cnf", Solver.SAT)

  testDIMACSFile("uf150-010.cnf", Solver.SAT)
  testDIMACSFile("uf150-027.cnf", Solver.SAT)

  testDIMACSFile("uuf150-011.cnf", Solver.UNSAT)
  testDIMACSFile("uuf150-024.cnf", Solver.UNSAT)
}
