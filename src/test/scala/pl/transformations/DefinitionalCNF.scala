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

package pl.transformations

import org.specs2.mutable._
import org.warthog.generic.formulas._
import org.warthog.pl.formulas.{PL, PLAtom}
import org.warthog.pl.parsers._
import pl.F

/**
 * Tests for Definitional CNF Transformation
 */
class DefinitionalCNF extends Specification {
  val (x, y, z) = (PLAtom("x"), PLAtom("y"), PLAtom("z"))
  val notX = -x
  val impl = Implication(x, y)
  val xor = Xor(x, y)
  val equiv = Equiv(x, y)
  val and = x && y && -z
  val or = -x || y || z
  val negand = -and
  val negor = -or

  "$true" should {
    "have a Tseitin CNF $true'" in {
      F.verum.pl.tseitinCNF must be equalTo F.verum.pl
    }
    "have a Plaisted Greenbaum CNF $true'" in {
      F.verum.pl.plaistedGreenbaumCNF must be equalTo F.verum.pl
    }
  }

  "$false" should {
    "have a Tseitin CNF $false'" in {
      F.falsum.pl.tseitinCNF must be equalTo F.falsum.pl
    }
    "have a Plaisted Greenbaum CNF $false'" in {
      F.falsum.pl.plaistedGreenbaumCNF must be equalTo F.falsum.pl
    }
  }

  "Variable x" should {
    "have a Tseitin CNF x'" in {
      F.x.pl.tseitinCNF must be equalTo F.x.pl
    }
    "have a Plaisted Greenbaum CNF x'" in {
      F.x.pl.plaistedGreenbaumCNF must be equalTo F.x.pl
    }
  }

  "~x" should {
    "have a Tseitin CNF ~x'" in {
      F.notx.pl.tseitinCNF must be equalTo F.notx.pl
    }
    "have a Plaisted Greenbaum CNF ~x'" in {
      F.notx.pl.plaistedGreenbaumCNF must be equalTo F.notx.pl
    }
  }

  "x & y" should {
    "have a Tseitin CNF ~x | y'" in {
      "x & y".pl.tseitinCNF must be equalTo "x & y".pl
    }
    "have a Plaisted Greenbaum CNF ~x | y'" in {
      "x & y".pl.plaistedGreenbaumCNF must be equalTo "x & y".pl
    }
  }

  "x <~> y" should {
    "have the right Tseitin CNF" in {
      val cnf = "x <~> y".pl.tseitinCNF
      cnf.isCNF must be equalTo true
      cnf.asInstanceOf[And[PL]].args.size must be equalTo 10
      cnf.vars must containAllOf(Seq(PLAtom("x"), PLAtom("y"), PLAtom("CNFVar0"), PLAtom("CNFVar1"), PLAtom("CNFVar2")))
    }
    "have the right Plaisted Greenbaum CNF" in {
      val cnf = "x <~> y".pl.plaistedGreenbaumCNF
      cnf.isCNF must be equalTo true
      cnf.asInstanceOf[And[PL]].args.size must be equalTo 6
      cnf.vars must containAllOf(Seq(PLAtom("x"), PLAtom("y"), PLAtom("CNFVar0"), PLAtom("CNFVar1"), PLAtom("CNFVar2")))
    }
  }

  "x <=> y" should {
    "have the right Tseitin CNF" in {
      val cnf = "x <=> y".pl.tseitinCNF
      cnf.isCNF must be equalTo true
      cnf.asInstanceOf[And[PL]].args.size must be equalTo 10
      cnf.vars must containAllOf(Seq(PLAtom("x"), PLAtom("y"), PLAtom("CNFVar0"), PLAtom("CNFVar1"), PLAtom("CNFVar2")))
    }
    "have the right Plaisted Greenbaum CNF" in {
      val cnf = "x <=> y".pl.plaistedGreenbaumCNF
      cnf.isCNF must be equalTo true
      cnf.asInstanceOf[And[PL]].args.size must be equalTo 6
      cnf.vars must containAllOf(Seq(PLAtom("x"), PLAtom("y"), PLAtom("CNFVar0"), PLAtom("CNFVar1"), PLAtom("CNFVar2")))
    }
  }

  "x & y & ~z y" should {
    "already be in CNF" in {
      "x & y & ~z".pl.tseitinCNF must be equalTo "x & y & ~z".pl
      "x & y & ~z".pl.plaistedGreenbaumCNF must be equalTo "x & y & ~z".pl
    }
  }

  "x | y | ~z y" should {
    "already be in CNF" in {
      "x | y | ~z".pl.tseitinCNF must be equalTo "x | y | ~z".pl
      "x | y | ~z".pl.plaistedGreenbaumCNF must be equalTo "x | y | ~z".pl
    }
  }

  "(a & ~b) | (c & ~d & e)" should {
    "have the right Tseitin CNF" in {
      val cnf = "(a & ~b) | (c & ~d & e)".pl.tseitinCNF
      cnf.isCNF must be equalTo true
      cnf.asInstanceOf[And[PL]].args.size must be equalTo 11
      cnf.vars must containAllOf(Seq(PLAtom("a"), PLAtom("b"), PLAtom("c"), PLAtom("d"), PLAtom("e"), PLAtom("CNFVar0"), PLAtom("CNFVar1"), PLAtom("CNFVar2")))
    }
    "have the right Plaisted Greenbaum CNF'" in {
      val cnf = "(a & ~b) | (c & ~d & e)".pl.plaistedGreenbaumCNF
      cnf.isCNF must be equalTo true
      cnf.asInstanceOf[And[PL]].args.size must be equalTo 7
      cnf.vars must containAllOf(Seq(PLAtom("a"), PLAtom("b"), PLAtom("c"), PLAtom("d"), PLAtom("e"), PLAtom("CNFVar0"), PLAtom("CNFVar1"), PLAtom("CNFVar2")))
    }
  }
}
