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
    val sol1 = "List((~D_2_2, D_3_6), (~D_3_6, D_2_6), (~D_3_6, ~x_3, D_2_2), (~D_2_6, x_3, D_3_6), (D_2_6), (~D_1_-1, D_2_2), (~D_2_2, D_1_2), (~D_2_2, ~x_2, D_1_-1), (~D_1_2, x_2, D_2_2), (D_1_2), (~D_1_-1))"
    "be equal to sol1" in {
      c1.encode().toString() must be equalTo sol1
    }
  }

  "1x_1 + 2x_2 + 3x_3 + 5x_4 <= 6" should {
    val c2 = new PBCtoSAT(List(1,2,3,5),6)
    val sol2 = "List((~D_3_1, D_4_6), (~D_4_6, D_3_6), (~D_4_6, ~x_4, D_3_1), (~D_3_6, x_4, D_4_6), (D_3_6), (~D_2_-2, D_3_1), (~D_3_1, D_2_1), (~D_3_1, ~x_3, D_2_-2), (~D_2_1, x_3, D_3_1), (~D_1_-1, D_2_1), (~D_2_1, D_1_1), (~D_2_1, ~x_2, D_1_-1), (~D_1_1, x_2, D_2_1), (D_1_1), (~D_1_-1), (~D_2_-2))"
    "be equal to sol2" in {
      c2.encode().toString() must be equalTo sol2
    }
  }

  "1x_1 + 2x_2 + 3x_3 + 6x_4 <= 6" should {
    val c3 = new PBCtoSAT(List(1,2,3,6),6)
    val sol3 = "List((~D_3_0, D_4_6), (~D_4_6, D_3_6), (~D_4_6, ~x_4, D_3_0), (~D_3_6, x_4, D_4_6), (D_3_6), (D_3_0, x_1, x_2, x_3), (~D_3_0, ~x_1), (~D_3_0, ~x_2), (~D_3_0, ~x_3))"
    "be equal to sol3" in {
      c3.encode().toString() must be equalTo sol3
    }
  }
}

