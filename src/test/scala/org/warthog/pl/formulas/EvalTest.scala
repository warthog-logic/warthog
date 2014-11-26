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

package org.warthog.pl.formulas

import org.specs2.mutable._
import org.warthog.pl.F
import org.warthog.pl.parsers.tptp._
import org.warthog.generic.formulas.And
import org.warthog.generic.formulas.Formula

/**
 * Basic tests for PLFormula.eval
 */
class EvalTest extends Specification {

  val m = Map[PLAtom,Boolean](
    PLAtom("x") -> true,
    PLAtom("y") -> false,
    PLAtom("z") -> true,
    PLAtom("w") -> false
  )

  // Atoms
  "$true" should {
    "should eval to true" in {
      F.verum.pl.eval(m) must be equalTo true
    }
  }

  "$false" should {
    "should eval to false" in {
      F.falsum.pl.eval(m) must be equalTo false
    }
  }

  "x" should {
    "should eval to true with assignment (x -> true)" in {
      F.x.pl.eval(m) must be equalTo true
    }
  }

  "y" should {
    "should eval to false with assignment (y -> false)" in {
      F.y.pl.eval(m) must be equalTo false
    }
  }

  "a" should {
    "should not be evaluated with no assignment" in {
      "a".pl.eval(m) should throwAn[Exception]("Variable a was not assigned")
    }
  }

  // Not
  "~x" should {
    "should eval to false with assignment (x -> true)" in {
      F.notx.pl.eval(m) must be equalTo false
    }
  }

  "~y" should {
    "should eval to true with assignment (y -> false)" in {
      F.noty.pl.eval(m) must be equalTo true
    }
  }

  // Implication
  "x => y" should {
    "should eval to false with assignment (x -> true, y -> false)" in {
      F.x_impl_y.pl.eval(m) must be equalTo false
    }
  }

  "x <= y" should {
    "should eval to false with assignment (x -> true, y -> false)" in {
      "x <= y".pl.eval(m) must be equalTo true
    }
  }

  "~x => y" should {
    "should eval to true with assignment (x -> true, y -> false)" in {
      "~x => y".pl.eval(m) must be equalTo true
    }
  }

  "~x <= y" should {
    "should eval to true with assignment (x -> true, y -> false)" in {
      "~x <= y".pl.eval(m) must be equalTo true
    }
  }

  // Xor
  "x <~> y" should {
    "should eval to true with assignment (x -> true, y -> false)" in {
      F.x_xor_y.pl.eval(m) must be equalTo true
    }
  }
  "~x <~> y" should {
    "should eval to false with assignment (x -> true, y -> false)" in {
      "~x <~> y".pl.eval(m) must be equalTo false
    }
  }

  // Equiv
  "x <=> y" should {
    "should eval to false with assignment (x -> true, y -> false)" in {
      F.x_equiv_y.pl.eval(m) must be equalTo false
    }
  }
  "~x <=> y" should {
    "should eval to true with assignment (x -> true, y -> false)" in {
      "~x <=> y".pl.eval(m) must be equalTo true
    }
  }
  "x <=> ~y" should {
    "should eval to true with assignment (x -> true, y -> false)" in {
      "x <=> ~y".pl.eval(m) must be equalTo true
    }
  }

  // And
  "x & y" should {
    "should eval to false with assignment (x -> true, y -> false)" in {
      F.xy.pl.eval(m) must be equalTo false
    }
  }

  "~x & x" should {
    "should eval to false with assignment (x -> true, y -> false)" in {
      "~x & x".pl.eval(m) must be equalTo false
    }
  }

  "x & y & ~z" should {
    "should eval to false with assignment (x -> true, y -> false, z -> true)" in {
      F.xynz.pl.eval(m) must be equalTo false
    }
  }

  "x & ~y & z" should {
    "should eval to true with assignment (x -> true, y -> false, z -> true)" in {
      "x & ~y & z".pl.eval(m) must be equalTo true
    }
  }

  "x & ~y & z & w" should {
    "should eval to false with assignment (x -> true, y -> false, z -> true, w -> false)" in {
      "x & ~y & z & w".pl.eval(m) must be equalTo false
    }
  }

  "x & ~y & z & ~w" should {
    "should eval to true with assignment (x -> true, y -> false, z -> true, w -> false)" in {
      "x & ~y & z & ~w".pl.eval(m) must be equalTo true
    }
  }

  // Or
  "x | y | z" should {
    "should eval to true with assignment (x -> true, y -> false, z -> true)" in {
      F.xoyoz.pl.eval(m) must be equalTo true
    }
  }

  "x | y" should {
    "should eval to true with assignment (x -> true, y -> false)" in {
      F.xoy.pl.eval(m) must be equalTo true
    }
  }

  "~x | y | ~z | w" should {
    "should eval to false with assignment (x -> true, y -> false, z -> true, w -> false)" in {
      "~x | y | ~z | w".pl.eval(m) must be equalTo false
    }
  }

  "~x | y | ~z | ~w" should {
    "should eval to true with assignment (x -> true, y -> false, z -> true, w -> false)" in {
      "~x | y | ~z | ~w".pl.eval(m) must be equalTo true
    }
  }

  // complex expressions
  "(x => (~y <=> ~z)) <= y" should {
    "should eval to true with assignment (x -> true, y -> false, z -> true)" in {
      F.equiv1_br.pl.eval(m) must be equalTo true
    }
  }

  "(x => (~y <=> ~z)) <= z" should {
    "should eval to false with assignment (x -> true, y -> false, z -> true)" in {
      "(x => (~y <=> ~z)) <= z".pl.eval(m) must be equalTo false
    }
  }

  "(x => (~y <~> ~z)) <= z" should {
    "should eval to true with assignment (x -> true, y -> false, z -> true)" in {
      "(x => (~y <~> ~z)) <= z".pl.eval(m) must be equalTo true
    }
  }



}
