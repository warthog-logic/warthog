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
import org.warthog.pl.formulas.PLAtom
import org.warthog.pl.parsers._


/**
 * Basic tests for propositional generic
 * + normal forms
 * + transformations & simplicifations
 *
 * Author: zengler
 * Date:   25.01.12
 */
class SubstitutionTest extends Specification {
  val (x, y, z) = (PLAtom("x"), PLAtom("y"), PLAtom("z"))

  "Substitution of x & y for z " should {
    "be $true for $true" in {
      "$true".pl.substitute(z, x && y) must be equalTo "$true".pl
    }
    "be $false for $false" in {
      "$false".pl.substitute(z, x && y) must be equalTo "$false".pl
    }
    "be x & y for z" in {
      "z".pl.substitute(z, x && y) must be equalTo "x & y".pl
    }
    "be ~(x & y) for ~z" in {
      "~z".pl.substitute(z, x && y) must be equalTo "~(x & y)".pl
    }
    "be (x | y) for (x | y)" in {
      "x | y".pl.substitute(z, x && y) must be equalTo "x | y".pl
    }
    "be (x & y) => y for z => y" in {
      "z => y".pl.substitute(z, x && y) must be equalTo "(x & y) => y".pl
    }
    "be (x & y) <=> y for z <=> y" in {
      "z <=> y".pl.substitute(z, x && y) must be equalTo "(x & y) <=> y".pl
    }
    "be (x & y) <~> y for z <~> y" in {
      "z <~> y".pl.substitute(z, x && y) must be equalTo "(x & y) <~> y".pl
    }
    "be x & y & x & y for z & z" in {
      "z & z".pl.substitute(z, x && y) must be equalTo "x & y & x & y".pl
    }
    "be x & y | x & y for z | z" in {
      "z | z".pl.substitute(z, x && y) must be equalTo "x & y | x & y".pl
    }
  }
}
