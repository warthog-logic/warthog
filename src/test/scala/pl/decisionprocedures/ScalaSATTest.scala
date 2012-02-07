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

import org.specs2.mutable.Specification
import org.warthog.pl.formulas.PLAtom
import org.warthog.pl.generators.{LangfordPairingGenerator, PigeonHoleGenerator}
import org.warthog.pl.decisionprocedures.satsolver.scalasat.ScalaSAT

/**
 * Tests for ScalaSAT
 *
 * Author: zengler
 * Date:   26.01.12
 */
class ScalaSATTest extends Specification {

  args(sequential = true)

  val a = PLAtom("a")
  val b = PLAtom("b")
  val x = PLAtom("x")

  "(-a), (a,b)" should {
    "be SAT" in {
      val s = ScalaSAT()
      s.add(-a && a || b)
      s.sat must be equalTo true
    }
  }

  "(-a), (a,b), (-b)" should {
    "be UNSAT" in {
      val s = ScalaSAT()
      s.add(-a && (a || b) && -b)
      s.sat must be equalTo false
    }
  }

  "(-a,-x),(a,-x),(-b,x),(b,x)" should {
    val s = ScalaSAT()
    s.add((-a || -x) && (a || -x) && (-b || x) && (b || x))
    "be UNSAT" in {
      s.sat must be equalTo false
    }
  }

  "pigeon hole problem of size 2" should {
    val pg = PigeonHoleGenerator.generate(2)
    val s = ScalaSAT()
    s.add(pg)
    "be UNSAT" in {
      s.sat must be equalTo false
    }
  }

  "pigeon hole problem of size 3" should {
    val pg = PigeonHoleGenerator.generate(3)
    val s = ScalaSAT()
    s.add(pg)
    "be UNSAT" in {
      s.sat must be equalTo false
    }
  }

  "pigeon hole problem of size 4" should {
    val pg = PigeonHoleGenerator.generate(4)
    val s = ScalaSAT()
    s.add(pg)
    "be UNSAT" in {
      s.sat must be equalTo false
    }
  }

  "pigeon hole problem of size 5" should {
    val pg = PigeonHoleGenerator.generate(5)
    val s = ScalaSAT()
    s.add(pg)
    "be UNSAT" in {
      s.sat must be equalTo false
    }
  }

  "pigeon hole problem of size 6" should {
    val pg = PigeonHoleGenerator.generate(6)
    val s = ScalaSAT()
    s.add(pg)
    "be UNSAT" in {
      s.sat must be equalTo false
    }
  }

  "pigeon hole problem of size 7" should {
    val pg = PigeonHoleGenerator.generate(7)
    val s = ScalaSAT()
    s.add(pg)
    "be UNSAT" in {
      s.sat must be equalTo false
    }
  }

  "pigeon hole problem of size 8" should {
    val pg = PigeonHoleGenerator.generate(8)
    val s = ScalaSAT()
    s.add(pg)
    "be UNSAT" in {
      s.sat must be equalTo false
    }
  }

  "langford pairing problem of size 1" should {
    val pg = LangfordPairingGenerator.generate(1)
    val s = ScalaSAT()
    s.add(pg)
    "be UNSAT" in {
      s.sat must be equalTo false
    }
  }

  "langford pairing problem of size 2" should {
    val pg = LangfordPairingGenerator.generate(2)
    val s = ScalaSAT()
    s.add(pg)
    "be UNSAT" in {
      s.sat must be equalTo false
    }
  }

  "langford pairing problem of size 3" should {
    val pg = LangfordPairingGenerator.generate(3)
    val s = ScalaSAT()
    s.add(pg)
    "be SAT" in {
      s.sat must be equalTo true
    }
  }

  "langford pairing problem of size 4" should {
    val pg = LangfordPairingGenerator.generate(4)
    val s = ScalaSAT()
    s.add(pg)
    "be SAT" in {
      s.sat must be equalTo true
    }
  }

  "langford pairing problem of size 5" should {
    val pg = LangfordPairingGenerator.generate(5)
    val s = ScalaSAT()
    s.add(pg)
    "be UNSAT" in {
      s.sat must be equalTo false
    }
  }

  "langford pairing problem of size 6" should {
    val pg = LangfordPairingGenerator.generate(6)
    val s = ScalaSAT()
    s.add(pg)
    "be UNSAT" in {
      s.sat must be equalTo false
    }
  }

  "langford pairing problem of size 7" should {
    val pg = LangfordPairingGenerator.generate(7)
    val s = ScalaSAT()
    s.add(pg)
    "be UNSAT" in {
      s.sat must be equalTo true
    }
  }

}
