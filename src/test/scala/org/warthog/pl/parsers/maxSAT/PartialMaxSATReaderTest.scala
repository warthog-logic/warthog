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

package org.warthog.pl.parsers.maxSAT

import org.warthog.pl.optimization.maxSAT.ExampleInstances._
import org.specs2.mutable.Specification
import org.warthog.generic.formulas.And

class PartialMaxSATReaderTest extends Specification {
  sequential
  val fs = System.getProperty("file.separator")

  val partialMaxSATReader = new PartialMaxSATReader()

  testPartialMaxSATReader("emptyAndNotEmptyClauses.wcnf", dirPartialMaxSATSimple, 4, 5, 2, 3)
  // TODO vars-method bug
  //testPartialMaxSATReader("emptyFormula.wcnf", dirPartialMaxSATSimple, 0, 0, 0, 0)
  testPartialMaxSATReader("f01.wcnf", dirPartialMaxSATSimple, 3, 4, 1, 3)
  testPartialMaxSATReader("f02.wcnf", dirPartialMaxSATSimple, 4, 5, 2, 3)
  testPartialMaxSATReader("f03.wcnf", dirPartialMaxSATSimple, 3, 8, 2, 6)
  testPartialMaxSATReader("f04.wcnf", dirPartialMaxSATSimple, 6, 17, 6, 11)
  testPartialMaxSATReader("f05.wcnf", dirPartialMaxSATSimple, 4, 10, 3, 7)
  testPartialMaxSATReader("f06.wcnf", dirPartialMaxSATSimple, 3, 4, 1, 3)
  testPartialMaxSATReader("f07.wcnf", dirPartialMaxSATSimple, 3, 7, 3, 4)
  testPartialMaxSATReader("f08.wcnf", dirPartialMaxSATSimple, 5, 10, 5, 5)
  testPartialMaxSATReader("f09.wcnf", dirPartialMaxSATSimple, 3, 6, 2, 4)
  testPartialMaxSATReader("f10.wcnf", dirPartialMaxSATSimple, 3, 9, 3, 6)
  testPartialMaxSATReader("f11.wcnf", dirPartialMaxSATSimple, 3, 6, 2, 4)
  testPartialMaxSATReader("oneClauseFormulaHard.wcnf", dirPartialMaxSATSimple, 3, 1, 1, 0)
  testPartialMaxSATReader("oneClauseFormulaSoft.wcnf", dirPartialMaxSATSimple, 3, 1, 0, 1)
  testPartialMaxSATReader("oneEmptyClauseHard.wcnf", dirPartialMaxSATSimple, 0, 1, 1, 0)
  testPartialMaxSATReader("oneEmptyClauseSoft.wcnf", dirPartialMaxSATSimple, 0, 1, 0, 1)
  testPartialMaxSATReader("oneVariableFormula.wcnf", dirPartialMaxSATSimple, 1, 3, 0, 3)
  testPartialMaxSATReader("oneVariableOneClauseFormulaHard.wcnf", dirPartialMaxSATSimple, 1, 1, 1, 0)
  testPartialMaxSATReader("oneVariableOneClauseFormulaSoft.wcnf", dirPartialMaxSATSimple, 1, 1, 0, 1)
  testPartialMaxSATReader("threeEmptyClauses.wcnf", dirPartialMaxSATSimple, 0, 3, 1, 2)

  def testPartialMaxSATReader(filename: String,
                              directory: String,
                              numVars: Int,
                              numClauses: Int,
                              numHardClauses: Int,
                              numSoftClauses: Int) =
    filename + " in " + directory should {
      partialMaxSATReader.read(directory + fs + filename)
      "have " + numVars + " variables" in {
        And(partialMaxSATReader.hardClauses.map(_.toFormula).union(
          partialMaxSATReader.softClauses.map(_.toFormula)): _*).vars.size must be equalTo numVars
      }
      "have " + numClauses + " clauses" in {
        (partialMaxSATReader.hardClauses.size + partialMaxSATReader.softClauses.size) must be equalTo numClauses
      }
      "have " + numHardClauses + " hard clauses" in {
        partialMaxSATReader.hardClauses.size must be equalTo numHardClauses
      }
      "have " + numSoftClauses + " soft clauses" in {
        partialMaxSATReader.softClauses.size must be equalTo numSoftClauses
      }
    }
}