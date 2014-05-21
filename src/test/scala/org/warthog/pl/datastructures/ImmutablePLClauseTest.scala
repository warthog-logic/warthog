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

package org.warthog.pl.datastructures

import org.specs2.mutable.Specification
import org.warthog.pl.parsers.tptp._
import org.warthog.pl.datastructures.cnf.{ImmutablePLClause => Clause, MutablePLClause, PLLiteral}

/**
  * Test for the immutable propositional clauses
  */

class ImmutablePLClauseTest extends Specification {

  val (x, y, z, nw) = (PLLiteral("x", true), PLLiteral("y", true), PLLiteral("z", true), PLLiteral("w", false))
  val cEmpty = new Clause()
  val cxyz = new Clause(x, y, z)
  val cyxz = new Clause(y, x, z)
  val cxy = new Clause(x, y)
  val cnwz = new Clause(nw, z)
  val cxyznw = new Clause(x, y, z, nw)
  val cxnx = new Clause(x, PLLiteral("x", false))
  val cx = new Clause(x)
  val cy = new Clause(y)
  val cz = new Clause(z)
  val cnw = new Clause(nw)
  val cyz = new Clause(z, y)

  "()" should {
    "be formula $false" in {
      cEmpty.toFormula must be equalTo "$false".pl
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
      cxyz.toFormula must be equalTo "x | y | z".pl
    }
  }

  "(x,y,z)" should {
    "have size 3" in {
      cxyz.size must be equalTo 3
    }
  }

  "(x,y,z)" should {
    "not be unit" in {
      cxyz.isUnit must be equalTo false
    }
  }

  "(x,y,z)" should {
    "be unit" in {
      cx.isUnit must be equalTo true
    }
  }

  "(x,y,z)" should {
    "not be empty" in {
      cxyz.isEmpty must be equalTo false
    }
  }

  "()" should {
    "be empty" in {
      cEmpty.isEmpty must be equalTo true
    }
  }

  "(x,y,z)" should {
    "have literals x, y, z" in {
      cxyz.literals must be equalTo List(x, y, z)
    }
  }

  "(x,y,z)" should {
    "be equals to (y,x,z)" in {
      cxyz == cyxz must be equalTo true
    }
  }

  "(x,y,z)" should {
    "should have the same hash code as (y,x,z)" in {
      cxyz.## == cyxz.## must be equalTo true
    }
  }

  "(x,y,z) - x" should {
    "be equals to (y,z)" in {
      cxyz.delete(x) must be equalTo new Clause(y, z)
    }
  }

  "~w::(x,y,z)" should {
    "be equals to (~w,x,y,z)" in {
      cxyz.push(nw) must be equalTo new Clause(x, y, z, nw)
    }
  }

  "x::(x,y,z)" should {
    "be equals to (x,y,z)" in {
      cxyz.push(x) must be equalTo cxyz
    }
  }

  "(x,y) ++ (~w,z)" should {
    "be equals to (~w,x,y,z)" in {
      cxy.union(cnwz) must be equalTo cxyznw
    }
  }

  "(x,y,z) ++ (y,x,z)" should {
    "be equals to (x,y,z)" in {
      cxyz.union(cyxz) must be equalTo cxyz
    }
  }

  "(x,~x)" should {
    "be a tautology" in {
      cxnx.isTautology must be equalTo true
    }
  }

  "(x,y,z,~w,~z)" should {
    "be a tautology" in {
      new Clause(x, y, z, nw, PLLiteral("z", false)).isTautology must be equalTo true
    }
  }

  "(x,y,z)/z" should {
    "be equal to (x,y)" in {
      cxyz.delete(z) == cxy must be equalTo true
    }
  }
  "z::(x,y,z)" should {
    "be equal to (x,y)" in {
      cxy.push(z) must be equalTo cxyz
    }
  }
  "(x,y,z)" should {
    "be equal to (y) ++ (x,z)" in {
      cy.pushLiterals(x, z) must be equalTo cxyz
    }
  }
  "(x,y,z)" should {
    "be equal to (x,y) ++ (z)" in {
      cxy.union(cz) must be equalTo cxyz
    }
  }
  "(x,y,z)" should {
    "be equal to (y,z) ++ (x)" in {
      cyz.union(cx) must be equalTo cxyz
    }
  }
  "(x,y,z)" should {
    "be equal to (x,y,z) ++ (x)" in {
      cxyz.union(cx) must be equalTo cxyz
    }
  }
  "(x,y,z)" should {
    "not be equal to (x,y,z) ++ (w)" in {
      cxyz.union(cnw) must not equalTo cxyz
    }
  }
  "(x,y,z)" should {
    "be equal to (x,y,z) ++ (w) / w" in {
      cxyz.union(cnw).delete(nw) must be equalTo cxyz
    }
  }

  "(x,y)" should {
    "have the premise (x,y)" in {
      new Clause(cxy).premise must be equalTo List()
    }
  }

  "(x,y)" should {
    "have the consequence ()" in {
      new Clause(cxy).consequence must be equalTo List(x,y)
    }
  }

  "(~w,z)" should {
    "have the premise (x,z)" in {
      new Clause(cnwz).premise must be equalTo List(nw)
    }
  }

  "(~w,z)" should {
    "have the consequence (~w)" in {
      new Clause(cnwz).consequence must be equalTo List(z)
    }
  }

}
