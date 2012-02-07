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

package pl.knowledgecompilation

import org.specs2.mutable._
import org.warthog.pl.knowledgecompilation.bdd.BDDManager
import org.warthog.pl.parsers._
import org.warthog.pl.formulas.PLAtom

/**
 * Tests for the internal BDD package
 *
 * Author: zengler
 * Date:   25.01.12
 */
class BDDTest extends Specification {
  val (a, b, c) = (PLAtom("a"), PLAtom("b"), PLAtom("c"))
  val m = new BDDManager(Seq(a, b, c))
  val contradiction1 = m.mkBDD("a & ~a".pl)
  val tautology = m.mkBDD("a | ~a".pl)
  val contradiction2 = m.mkBDD("(a => b) & (b => c) & a & ~c".pl)
  val ex0 = m.bddExists(m.mkBDD("a | b | c".pl), Set(b))
  val ex1 = m.bddExists(m.mkBDD("a & b & c".pl), Set(b))
  val all0 = m.bddForAll(m.mkBDD("a & b & c".pl), Set(b))
  val all1 = m.bddForAll(m.mkBDD("a | b | c".pl), Set(b))

  "a & ~b" should {
    "be a contradiction" in {
      m.isContradiction(contradiction1) must be equalTo true
    }
    "not be a tautology" in {
      m.isTautology(contradiction1) must be equalTo false
    }
  }

  "(a => b) & (b => c) & a & ~c)" should {
    "be a contradiction" in {
      m.isContradiction(contradiction2) must be equalTo true
    }
    "not be a tautology" in {
      m.isTautology(contradiction2) must be equalTo false
    }
  }

  "a | ~a" should {
    "be a tautology" in {
      m.isTautology(tautology) must be equalTo true
    }
    "not be a contradiction" in {
      m.isContradiction(tautology) must be equalTo false
    }
  }

  "?[b]: a | b | c" should {
    "be a tautology" in {
      m.isTautology(ex0) must be equalTo true
    }
  }

  "![b]: a | b | c" should {
    "be equal to a | c" in {
      m.toFormula(all1) must be equalTo "a & $true | (~a & (c & $true | ~c & ~$true))".pl
    }
  }

  "?[b]: a & b & c" should {
    "be equal to a & c" in {
      m.toFormula(ex1) must be equalTo "a & (c & $true | ~c & ~$true) | ~a & ~$true".pl
    }
  }

  "![b]: a & b & c" should {
    "be a contradiction" in {
      m.isContradiction(all0) must be equalTo true
    }
  }
}
