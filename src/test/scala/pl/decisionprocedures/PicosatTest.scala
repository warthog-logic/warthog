/**
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

package pl.decisionprocedures

import org.warthog.pl.decisionprocedures.satsolver.{Infinity, Solver, sat}
import org.specs2.mutable.Specification
import org.warthog.pl.formulas.{PL, PLAtom}
import org.warthog.generic.formulas.{Formula, Verum, Falsum}
import org.warthog.pl.decisionprocedures.satsolver.impl.picosat.Picosat

/**
 * Tests for the picosat bindings
 *
 * Author: kuebler
 * Date:   25.01.12
 */
class PicosatTest extends Specification {

  val (x, y, z) = (PLAtom("x"), PLAtom("y"), PLAtom("z"))
  val ps = new Picosat
  var rv0: Int = _
  var rv1: Int = _
  var fm: Formula[PL] = _

  /*
   * By default, tests are executed concurrently. JNI/JNA, however, is able to load _only one_ instance of
   * (lib)picosat.{so,dylib,dll} per JVM so concurrently accessing the picosat INSTANCE will result in double
   * instantiation errors and unexpected behaviour.
   */
  args(sequential = true)

  //TODO property file

  "x" should {
    "be satisfiable" in {
      sat(ps) {
        (solver: Solver) => {
          solver.add(x)
          rv0 = solver.sat(Infinity)
        }
      }
      rv0 must be equalTo (1)
    }
    "be satisfied by model x" in {
      sat(ps) {
        (solver: Solver) => {
          solver.add(x)
          solver.sat(Infinity)
          fm = solver.getModel()
        }
      }
      fm must be equalTo (x)
    }
    "be unsatisfiable after adding -x" in {
      sat(ps) {
        solver => {
          solver.add(x)
          solver.add(-x)
          rv0 = solver.sat(Infinity)
        }
      }
      rv0 must be equalTo (-1)
    }
    "be unsatisfiable after adding -x, satisfiable again after dropping -x" in {
      sat(ps) {
        solver => {
          solver.add(x)
          solver.mark()
          solver.add(-x)
          rv0 = solver.sat(Infinity)
          solver.undo()
          rv1 = solver.sat(Infinity)
        }
      }
      (rv0 == -1 && rv1 == 1) must be equalTo (true)
    }
  }
  "the empty clause" should {
    "be satisfiable" in {
      sat(ps) {
        s => {
          s.add(Falsum())
          rv0 = s.sat(Infinity)
        }
      }
      rv0 must be equalTo (-1)
    }
  }
  "the empty formula" should {
    "be satisfiable" in {
      sat(ps) {
        s => {
          s.add(Verum())
          rv0 = s.sat(Infinity)
        }
      }
      rv0 must be equalTo (1)
    }
  }
  "the empty formula" should {
    "return true uppon sat checking" in {
      var model: Formula[PL] = null
      sat(ps) {
        s => {
          s.add(Verum())
          rv0 = s.sat(Infinity)
          model = s.getModel()
        }
      }
      model must be equalTo(Verum())
    }
  }
}
