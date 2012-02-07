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
import org.warthog.pl.formulas.PLAtom
import org.warthog.pl.parsers._
import pl.F

/**
 * Tests for CNF and DNF Conversion
 *
 * Author: zengler
 * Date:   25.01.12
 */
class CNFDNFTest extends Specification {
  val (x,y,z) = (PLAtom("x"), PLAtom("y"), PLAtom("z"))
  val notX = -x
  val impl = Implication(x, y)
  val xor = Xor(x, y)
  val equiv = Equiv(x, y)
  val and = x && y && -z
  val or = -x || y || z
  val negand = -and
  val negor = -or

  "$true" should {
    "be in CNF'" in {
      F.verum.pl.isCNF must be equalTo true
    }
    "be in DNF" in {
      F.verum.pl.isDNF must be equalTo true
    }
    "have CNF $true" in {
      F.verum.pl.cnf must be equalTo F.verum.pl
    }
    "have a simplified CNF $true" in {
      F.verum.pl.simplifiedCNF must be equalTo F.verum.pl
    }
    "have DNF $true" in {
      F.verum.pl.dnf must be equalTo F.verum.pl
    }
    "have a simplified DNF $true" in {
      F.verum.pl.simplifiedDNF must be equalTo F.verum.pl
    }
  }

  "$false" should {
    "be in CNF'" in {
      F.falsum.pl.isCNF must be equalTo true
    }
    "be in DNF" in {
      F.falsum.pl.isDNF must be equalTo true
    }
    "have CNF $false" in {
      F.falsum.pl.cnf must be equalTo F.falsum.pl
    }
    "have a simplified CNF $false" in {
      F.falsum.pl.simplifiedCNF must be equalTo F.falsum.pl
    }
    "have DNF $false" in {
      F.falsum.pl.dnf must be equalTo F.falsum.pl
    }
    "have a simplified DNF $false" in {
      F.falsum.pl.simplifiedDNF must be equalTo F.falsum.pl
    }
  }

  "Variable x" should {
    "be in CNF'" in {
      F.x.pl.isCNF must be equalTo true
    }
    "be in DNF" in {
      F.x.pl.isDNF must be equalTo true
    }
    "have CNF x" in {
      F.x.pl.cnf must be equalTo F.x.pl
    }
    "have a simplified CNF x" in {
      F.x.pl.simplifiedCNF must be equalTo F.x.pl
    }
    "have DNF x" in {
      F.x.pl.dnf must be equalTo F.x.pl
    }
    "have a simplified DNF x" in {
      F.x.pl.simplifiedDNF must be equalTo F.x.pl
    }
  }

  "~x" should {
    "be in CNF'" in {
      F.notx.pl.isCNF must be equalTo true
    }
    "be in DNF" in {
      F.notx.pl.isDNF must be equalTo true
    }
    "have CNF ~x" in {
      F.notx.pl.cnf must be equalTo F.notx.pl
    }
    "have DNF ~x" in {
      F.notx.pl.dnf must be equalTo F.notx.pl
    }
  }

  "x => y" should {
    "not be in CNF'" in {
      F.x_impl_y.pl.isCNF must be equalTo false
    }
    "not be in DNF" in {
      F.x_impl_y.pl.isDNF must be equalTo false
    }
    "have CNF ~x | y" in {
      F.x_impl_y.pl.cnf must be equalTo "~x | y".pl
    }
    "have a simplified CNF ~x | y" in {
      F.x_impl_y.pl.simplifiedCNF must be equalTo "~x | y".pl
    }
    "have DNF ~x | y" in {
      F.x_impl_y.pl.dnf must be equalTo "~x | y".pl
    }
    "have a simplified DNF ~x | y" in {
      F.x_impl_y.pl.simplifiedDNF must be equalTo "~x | y".pl
    }
  }

  "x <~> y" should {
    "not be in CNF'" in {
      F.x_xor_y.pl.isCNF must be equalTo false
    }
    "not be in DNF" in {
      F.x_xor_y.pl.isDNF must be equalTo false
    }
    "have CNF (~x | x) & (y | x) & (~x | ~y) & (y | ~y)" in {
      F.x_xor_y.pl.cnf must be equalTo "(~x | x) & (y | x) & (~x | ~y) & (y | ~y)".pl
    }
    "have a simplified CNF (y | x) & (~x | ~y)" in {
      F.x_xor_y.pl.simplifiedCNF must be equalTo "(y | x) & (~x | ~y)".pl
    }
    "have DNF x & ~y | ~x & y" in {
      F.x_xor_y.pl.dnf must be equalTo "x & ~y | ~x & y".pl
    }
    "have a simplified DNF x & ~y | ~x & y" in {
      F.x_xor_y.pl.simplifiedDNF must be equalTo "x & ~y | ~x & y".pl
    }
  }

  "x <=> y" should {
    "not be in CNF'" in {
      F.x_equiv_y.pl.isCNF must be equalTo false
    }
    "not be in DNF" in {
      F.x_equiv_y.pl.isDNF must be equalTo false
    }
    "have CNF (~x | x) & (~y | x) & (~x | y) & (~y | y)" in {
      F.x_equiv_y.pl.cnf must be equalTo "(~x | x) & (~y | x) & (~x | y) & (~y | y)".pl
    }
    "have a simplified CNF (~y | x) & (~x | y)" in {
      F.x_equiv_y.pl.simplifiedCNF must be equalTo "(~y | x) & (~x | y)".pl
    }
    "have DNF x & y | ~x & ~y" in {
      F.x_equiv_y.pl.dnf must be equalTo "x & y | ~x & ~y".pl
    }
    "have a simplified DNF x & y | ~x & ~y" in {
      F.x_equiv_y.pl.simplifiedDNF must be equalTo "x & y | ~x & ~y".pl
    }
  }

  "x & y & ~z" should {
    "be in CNF'" in {
      F.xynz.pl.isCNF must be equalTo true
    }
    "be in DNF" in {
      F.xynz.pl.isDNF must be equalTo true
    }
    "have CNF x & y & ~z" in {
      F.xynz.pl.cnf must be equalTo F.xynz.pl
    }
    "have a simplified CNF x & y & ~z" in {
      F.xynz.pl.simplifiedCNF must be equalTo F.xynz.pl
    }
    "have DNF x & y & ~z" in {
      F.xynz.pl.dnf must be equalTo F.xynz.pl
    }
    "have a simplified DNF x & y & ~z" in {
      F.xynz.pl.simplifiedDNF must be equalTo F.xynz.pl
    }
  }

  "~x | y | z" should {
    "be in CNF'" in {
      F.nxoyoz.pl.isCNF must be equalTo true
    }
    "be in DNF" in {
      F.nxoyoz.pl.isDNF must be equalTo true
    }
    "have CNF ~x | y | z" in {
      F.nxoyoz.pl.cnf must be equalTo F.nxoyoz.pl
    }
    "have a simplified CNF ~x | y | z" in {
      F.nxoyoz.pl.simplifiedCNF must be equalTo F.nxoyoz.pl
    }
    "have DNF ~x | y | z" in {
      F.nxoyoz.pl.dnf must be equalTo F.nxoyoz.pl
    }
    "have a simplified DNF ~x | y | z" in {
      F.nxoyoz.pl.simplifiedDNF must be equalTo F.nxoyoz.pl
    }
  }

  "~(x & y & ~z)" should {
    val negandNF = "~x | ~y | z"
    "not be in CNF'" in {
      F.n_xynz.pl.isCNF must be equalTo false
    }
    "not be in DNF" in {
      F.n_xynz.pl.isDNF must be equalTo false
    }
    "have CNF ~x | ~y | z" in {
      F.n_xynz.pl.cnf must be equalTo negandNF.pl
    }
    "have a simplified CNF ~x | ~y | z" in {
      F.n_xynz.pl.simplifiedCNF must be equalTo negandNF.pl
    }
    "have DNF ~x | ~y | z" in {
      F.n_xynz.pl.dnf must be equalTo negandNF.pl
    }
    "have a simplified DNF ~x | ~y | z" in {
      F.n_xynz.pl.simplifiedDNF must be equalTo negandNF.pl
    }
  }

  "~(~x | y | z)" should {
    val negorNF = "x & ~y & ~z"
    "not be in CNF'" in {
      F.n_nxoyoz.pl.isCNF must be equalTo false
    }
    "not be in DNF" in {
      F.n_nxoyoz.pl.isDNF must be equalTo false
    }
    "have CNF x & ~y & ~z" in {
      F.n_nxoyoz.pl.cnf must be equalTo negorNF.pl
    }
    "have a simplified CNF x & ~y & ~z" in {
      F.n_nxoyoz.pl.simplifiedCNF must be equalTo negorNF.pl
    }
    "have DNF x & ~y & ~z" in {
      F.n_nxoyoz.pl.dnf must be equalTo negorNF.pl
    }
    "have a simplified DNF x & ~y & ~z" in {
      F.n_nxoyoz.pl.simplifiedDNF must be equalTo negorNF.pl
    }
  }
}
