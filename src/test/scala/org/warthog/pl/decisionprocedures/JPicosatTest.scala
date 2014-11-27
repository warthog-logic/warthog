/*
 * Copyright (c) 2011-2014, Andreas J. Kuebler & Christoph Zengler & Rouven Walter
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

package org.warthog.pl.decisionprocedures

import org.specs2.mutable.Specification
import org.warthog.pl.decisionprocedures.satsolver.impl.picosat.JPicosat

class JPicosatTest extends Specification {
  /*
   * By default, tests are executed concurrently. JNI/JNA, however, is able to load _only one_ instance of
   * (lib)picosat.{so,dylib,dll} per JVM so concurrently accessing the picosat INSTANCE will result in double
   * instantiation errors and unexpected behaviour.
   */
  args(sequential = true)

  "JPicosat with -1 and 1 clauses" should {
    "be unsatisfiable" in {
      val jPicosat = new JPicosat()
      jPicosat.picosat_init()
      jPicosat.picosat_add(-1)
      jPicosat.picosat_add(0)
      jPicosat.picosat_add(1)
      jPicosat.picosat_add(0)
      jPicosat.picosat_sat(-1) must be equalTo JPicosat.PSUNSAT
    }
  }

  "JPicosat with -1 and 2 clauses" should {
    "be satisfiable" in {
      val jPicosat = new JPicosat()
      jPicosat.picosat_init()
      jPicosat.picosat_add(-1)
      jPicosat.picosat_add(0)
      jPicosat.picosat_add(2)
      jPicosat.picosat_add(0)
      jPicosat.picosat_sat(-1) must be equalTo JPicosat.PSSAT
    }
  }

  "JPicosat after reset and added -1 and 2 clauses" should {
    "be satisfiable" in {
      val jPicosat = new JPicosat()

      jPicosat.picosat_init()
      jPicosat.picosat_add(-1)
      jPicosat.picosat_add(0)
      jPicosat.picosat_add(1)
      jPicosat.picosat_add(0)
      jPicosat.picosat_sat(-1)
      jPicosat.picosat_reset()

      jPicosat.picosat_init()
      jPicosat.picosat_add(-1)
      jPicosat.picosat_add(0)
      jPicosat.picosat_add(2)
      jPicosat.picosat_add(0)
      jPicosat.picosat_sat(-1) must be equalTo JPicosat.PSSAT
    }
  }
}
