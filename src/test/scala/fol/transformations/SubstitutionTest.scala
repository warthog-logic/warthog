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
import org.warthog.fol.formulas.FOLVariable
import org.warthog.fol.parsers.tptp._

/**
 * Test cases for substitutions in first order logic
 *
 * Author: zengler
 * Date:   01.02.12
 */
class SubstitutionTest extends Specification {
  val X = FOLVariable("X")
  val Y = FOLVariable("Y")
  "Substitution of y with x" should {
    "yield ![X0]: r(X0,X) for ![X]: r(X,Y)" in {
      "![X]: r(X,Y)".fol.substitute(Y, X) must be equalTo "![X0]: r(X0,X)".fol
    }
    "yield ![X1,X0]: r(X1,X) => r(X1,X0) for ![X,X0]: r(X,Y) => r(X,X0)]" in {
      "![X,X0]: r(X,Y) => r(X,X0)".fol.substitute(Y, X) must be equalTo "![X1,X0]: r(X1,X) => r(X1,X0)".fol
    }
  }
}
