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

package org.warthog.pl.generators

import org.specs2.mutable.Specification
import org.warthog.pl.datastructures.cnf.{ PLLiteral, MutablePLClause => MClause, ImmutablePLClause => IMClause }

/**
 * Test for the PBCtoSAT encoding
 */
class PBCtoSATTest extends Specification {

  "2x_1 + 3x_2 + 4x_3 <= 6" should {
    val c1 = new PBCtoSAT(List(2,3,4),6)
    val sol = "List((~D_2,2, D_3,6), (~D_3,6, D_2,6), (~D_3,6, ~x_3, D_2,2), (~D_2,6, x_3, D_3,6), (D_2,6), (~D_1,-1, D_2,2), (~D_2,2, D_1,2), (~D_2,2, ~x_2, D_1,-1), (~D_1,2, x_2, D_2,2), (D_1,2), (~D_1,-1))"
    "be equal to m1" in {
      c1.encode().toString() must be equalTo sol
    }
  }
}

