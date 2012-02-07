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

package fol.transformations

import org.specs2.mutable._
import fol.F
import org.warthog.fol.formulas.FOLVariable
import org.warthog.fol.parsers.tptp._

/**
 * Test cases for PNF transformation and matrix of first order generic
 *
 * Author: zengler
 * Date:   01.02.12
 */
class PNFTest extends Specification {
  val X = FOLVariable("X")
  val Y = FOLVariable("Y")
  val Z = FOLVariable("Z")
  val U = FOLVariable("U")
  val V = FOLVariable("V")

  "x" should {
    "be in PNF" in {
      F.x.fol.isPNF must be equalTo true
    }
    "have a PNF of x" in {
      F.x.fol.pnf must be equalTo F.x.fol
    }
    "have a matrix of x" in {
      F.x.fol.matrix must be equalTo F.x.fol
    }
  }

  F.h144 should {
    val h144mat = "~p(X0) & ~r(Y) | q(Y0) | ~p(Z1) | ~q(Z1)"
    val h144pnf = "?[X0,Y0]: ![Z1]: " + h144mat
    "not be in PNF" in {
      F.h144.fol.isPNF must be equalTo false
    }
    "have a PNF of " + h144pnf in {
      F.h144.fol.pnf must be equalTo h144pnf.fol
    }
    "have a matrix of " + h144mat in {
      F.h144.fol.matrix must be equalTo h144mat.fol
    }
  }

  F.h150_1 should {
    val h150_1mat = "~(X < Y) | mul(X,U0) < mul(Y,V0)"
    val h150_1pnf = "?[Y]: ![U0]: ?[V0]: " + h150_1mat
    "not be in PNF" in {
      F.h150_1.fol.isPNF must be equalTo false
    }
    "have a PNF of " + h150_1pnf in {
      F.h150_1.fol.pnf must be equalTo h150_1pnf.fol
    }
    "have a matrix of " + h150_1mat in {
      F.h150_1.fol.matrix must be equalTo h150_1mat.fol
    }
  }

  F.h150_2 should {
    val h150_2mat = "~p(X) | q(Y0) | ~p(Z0) | ~q(Z0)"
    val h150_2pnf = "![X]: ![Z0]: ?[Y0]: " + h150_2mat
    "not be in PNF" in {
      F.h150_2.fol.isPNF must be equalTo false
    }
    "have a PNF of " + h150_2pnf in {
      F.h150_2.fol.pnf must be equalTo h150_2pnf.fol
    }
    "have a matrix of " + h150_2mat in {
      F.h150_2.fol.matrix must be equalTo h150_2mat.fol
    }
  }
}
