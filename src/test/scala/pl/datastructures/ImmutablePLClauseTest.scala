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
import org.warthog.pl.parsers._
import org.warthog.pl.datastructures.cnf.{PLLiteral, ImmutablePLClause => Clause}

/**
* Test for the immutable propositional clauses
*
* Author: zengler
* Date:   10.05.12
*/

class ImmutablePLClauseTest extends Specification {

  val (x, y, z, nw) = (PLLiteral("x", true), PLLiteral("y", true), PLLiteral("z", true), PLLiteral("w", false))
  val c1 = new Clause(x, y, z)
  val c2 = new Clause(y, x, z)
  val c3 = new Clause(x, y)
  val c4 = new Clause(nw, z)
  val c5 = new Clause(x, y, z, nw)
  val c6 = new Clause(x, PLLiteral("x", false))

  "()" should {
    "be formula $false" in {
      new Clause().toFormula must be equalTo "$false".pl
    }
  }

  "(x)" should {
    "be formula x" in {
      new Clause(x).toFormula must be equalTo "x".pl
    }
  }

  "(~x)" should {
    "be formula ~x" in {
      new Clause(PLLiteral("x", false)).toFormula must be equalTo "~x".pl
    }
  }

  "(x,y,z)" should {
    "be formula x | y | y" in {
      c1.toFormula must be equalTo "x | y | z".pl
    }
  }

  "(x,y,z)" should {
    "have size 3" in {
      c1.size must be equalTo 3
    }
  }

  "(x,y,z)" should {
    "not be unit" in {
      c1.isUnit must be equalTo false
    }
  }

  "(x,y,z)" should {
    "not be empty" in {
      c1.isEmpty must be equalTo false
    }
  }

  "(x,y,z)" should {
    "have literals x, y, z" in {
      c1.literals must be equalTo List(x, y, z)
    }
  }

  "(x,y,z)" should {
    "be equals to (y,x,z)" in {
      c1 == c2 must be equalTo true
    }
  }

  "(x,y,z)" should {
    "should have the same hash code as (y,x,z)" in {
      c1.## == c2.## must be equalTo true
    }
  }

  "(x,y,z) - x" should {
    "be equals to (y,z)" in {
      c1.delete(x) must be equalTo new Clause(y, z)
    }
  }

  "~w::(x,y,z)" should {
    "be equals to (~w,x,y,z)" in {
      c1.push(nw) must be equalTo new Clause(x, y, z, nw)
    }
  }

  "x::(x,y,z)" should {
    "be equals to (x,y,z)" in {
      c1.push(x) must be equalTo c1
    }
  }

  "(x,y) ++ (~w,z)" should {
    "be equals to (~w,x,y,z)" in {
      c3.union(c4) must be equalTo c5
    }
  }

  "(x,y,z) ++ (y,x,z)" should {
    "be equals to (x,y,z)" in {
      c1.union(c2) must be equalTo c1
    }
  }

  "(x,~x)" should {
    "be a tautology" in {
      c6.isTautology must be equalTo true
    }
  }

  "(x,y,z,~w,~z)" should {
    "be a tautology" in {
      new Clause(x, y, z, nw, PLLiteral("z", false)).isTautology must be equalTo true
    }
  }

}
