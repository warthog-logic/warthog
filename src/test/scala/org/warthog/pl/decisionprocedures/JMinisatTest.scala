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
import org.warthog.pl.decisionprocedures.satsolver.impl.minisat.JMinisat

class JMinisatTest extends Specification {
  /*
   * By default, tests are executed concurrently. JNI/JNA, however, is able to load _only one_ instance of
   * (lib)minisat.{so,dylib,dll} per JVM so concurrently accessing the minisat INSTANCE will result in double
   * instantiation errors and unexpected behaviour.
   */
  args(sequential = true)

  "JMinisat" should {
    val instance = new JMinisat()

    "be unsatisfiable with -1 and 1 added" in {
      val prover = instance.minisat_init()
      instance.minisat_add(prover, Array(-1))
      instance.minisat_add(prover, Array(1))
      val result = instance.minisat_sat(prover)
      instance.minisat_free(prover)
      result must be equalTo JMinisat.MSUNSAT
    }

    "be satisfiable with -1 and 2 added" in {
      val prover = instance.minisat_init()
      instance.minisat_add(prover, Array(-1))
      instance.minisat_add(prover, Array(2))
      val result = instance.minisat_sat(prover)
      instance.minisat_free(prover)
      result must be equalTo JMinisat.MSSAT
    }

    "should be not in conflict when creating two different pointers" in {
      val instance = new JMinisat()
      val prover01 = instance.minisat_init()
      val prover02 = instance.minisat_init()

      instance.minisat_add(prover01, Array(1))
      instance.minisat_add(prover01, Array(-1))

      instance.minisat_add(prover02, Array(1))
      instance.minisat_add(prover02, Array(2))

      val result01 = instance.minisat_sat(prover01)
      val result02 = instance.minisat_sat(prover02)

      instance.minisat_free(prover01)
      instance.minisat_free(prover02)

      result01 must be equalTo JMinisat.MSUNSAT
      result02 must be equalTo JMinisat.MSSAT
    }
  }
}