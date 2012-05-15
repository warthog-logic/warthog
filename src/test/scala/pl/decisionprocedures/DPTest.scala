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
import pl.F
import org.warthog.pl.parsers._
import org.warthog.pl.decisionprocedures.dpll.DPLL
import org.warthog.pl.generators.PigeonHoleGenerator

/**
 * Tests for the DPLL implementation
 *
 * Author: zengler
 * Date:   10.05.12
 */

class DPTest extends Specification {

  F.verum should {
    "be satisfiable" in {
      DPLL.dp(F.verum.pl) must be equalTo true
    }
  }

  F.falsum should {
    "be unsatisfiable" in {
      DPLL.dp(F.falsum.pl) must be equalTo false
    }
  }

  F.x should {
    "be satisfiable" in {
      DPLL.dp(F.x.pl) must be equalTo true
    }
  }

  F.x_impl_y should {
    "be satisfiable" in {
      DPLL.dp(F.x_impl_y.pl) must be equalTo true
    }
  }

  F.x_equiv_y should {
    "be satisfiable" in {
      DPLL.dp(F.x_equiv_y.pl) must be equalTo true
    }
  }

  F.x_xor_y should {
    "be satisfiable" in {
      DPLL.dp(F.x_xor_y.pl) must be equalTo true
    }
  }

  F.xyz should {
    "be satisfiable" in {
      DPLL.dp(F.xyz.pl) must be equalTo true
    }
  }

  F.n_nxoyoz should {
    "be satisfiable" in {
      DPLL.dp(F.n_nxoyoz.pl) must be equalTo true
    }
  }

  F.equivxor1_br should {
    "be satisfiable" in {
      DPLL.dp(F.equivxor1_br.pl) must be equalTo true
    }
  }

  "a & (a => b) & (b => c) & ~c" should {
    "be unsatisfiable" in {
      DPLL.dp("a & (a => b) & (b => c) & ~c".pl) must be equalTo false
    }
  }

  "pigeon hole of size 1" should {
    "be unsatisfiable" in {
      DPLL.dp(PigeonHoleGenerator.generate(1)) must be equalTo false
    }
  }

  "pigeon hole of size 2" should {
    "be unsatisfiable" in {
      DPLL.dp(PigeonHoleGenerator.generate(2)) must be equalTo false
    }
  }

  "pigeon hole of size 3" should {
    "be unsatisfiable" in {
      DPLL.dp(PigeonHoleGenerator.generate(3)) must be equalTo false
    }
  }
}
