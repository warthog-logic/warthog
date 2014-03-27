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

package pl.knowledgecompilation.dnnf

import org.warthog.generic.formulas.{Xor, Formula}
import org.warthog.pl.formulas.PL
import org.warthog.pl.knowledgecompilation.dnnf._
import org.warthog.pl.parsers._
import pl.F
import org.specs2.mutable._
import org.specs2.specification._
import org.warthog.pl.decisionprocedures.satsolver.impl.picosat.Picosat
import org.warthog.pl.decisionprocedures.satsolver.{Infinity, Duration, sat}

/**
  * Tests for DNNF-Compilation and Operations
  */

class DNNFCompilationTests extends Specification {

  val ps = new Picosat

  args(sequential = true)

  def checkEquality(formula: Formula[PL], dnnf: DNNF, ps: Picosat, duration: Long = -1): Int = {
    val checkFormula = Xor(formula, dnnf.asPLFormula).tseitinCNF
    var result = 0
    sat(ps) {
      solver =>
      {
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
      ("return true using the Simple Compiler") in {
        checkEquality(f, compile(Simple, f), ps) must be greaterThan 0
      }
      ("return true using the Advanced Compiler") in {
        checkEquality(f, compile(Advanced, f), ps) must be greaterThan 0
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
}
