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

package pl.datastructures

import org.specs2.mutable.Specification
import org.warthog.pl.datastructures.cnf.{PLLiteral, MutablePLClause => MClause, ImmutablePLClause => IMClause}

/**
 * Test for the propositional clauses
 *
 * Author: zengler
 * Date:   15.05.12
 */

class PLClauseTest extends Specification {

  val (x, y, z, nw) = (PLLiteral("x", true), PLLiteral("y", true), PLLiteral("z", true), PLLiteral("w", false))
  val m1 = new MClause(x, y, z)
  val m2 = new MClause(y, x, z)
  val m3 = new MClause(x, y)

  val im1 = new IMClause(x, y, z)
  val im2 = new IMClause(y, x, z)
  val im3 = new IMClause(x, y)


  "m1" should {
    "be equal to m1" in {
      m1 == m1 must be equalTo true
    }
    "be equal to m2" in {
      m1 == m2 must be equalTo true
    }
    "not be equal to m3" in {
      m1 == m3 must be equalTo false
    }
    "be equal to c1" in {
      m1 == im1 must be equalTo true
    }
    "be equal to c2" in {
      m1 == im2 must be equalTo true
    }
    "not be equal to c3" in {
      m1 == im3 must be equalTo false
    }
  }

  "im1" should {
    "be equal to im1" in {
      im1 == im1 must be equalTo true
    }
    "be equal to im2" in {
      im1 == im2 must be equalTo true
    }
    "not be equal to im3" in {
      im1 == im3 must be equalTo false
    }
    "be equal to c1" in {
      im1 == m1 must be equalTo true
    }
    "be equal to c2" in {
      im1 == m2 must be equalTo true
    }
    "not be equal to c3" in {
      im1 == m3 must be equalTo false
    }
  }

  "im1 as mutable clause" should {
    "be equal to m1" in {
      new MClause(im1) == m1 must be equalTo true
    }
    "be equal to m2" in {
      new MClause(im1) == m2 must be equalTo true
    }
    "not be equal to m3" in {
      new MClause(im1) == m3 must be equalTo false
    }
    "be equal to im1" in {
      new MClause(im1) == im1 must be equalTo true
    }
    "be equal to im2" in {
      new MClause(im1) == im2 must be equalTo true
    }
    "not be equal to im3" in {
      new MClause(im1) == im3 must be equalTo false
    }
  }

  "m1 as immutable clause" should {
    "be equal to im1" in {
      new IMClause(m1) == im1 must be equalTo true
    }
    "be equal to im2" in {
      new IMClause(m1) == im2 must be equalTo true
    }
    "not be equal to im3" in {
      new IMClause(m1) == im3 must be equalTo false
    }
    "be equal to m1" in {
      new IMClause(m1) == m1 must be equalTo true
    }
    "be equal to m2" in {
      new IMClause(m1) == m2 must be equalTo true
    }
    "not be equal to m3" in {
      new IMClause(m1) == m3 must be equalTo false
    }
  }
}
