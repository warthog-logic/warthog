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
import collection.mutable.ListBuffer

class PartialWeightedMaxSATReaderTest extends Specification {
  sequential
  val fs = System.getProperty("file.separator")

  val partialWeightedMaxSATReader = new PartialWeightedMaxSATReader()

  // Partial Weighted MaxSAT
  testPartialWeightedMaxSATReader("emptyAndNotEmptyClauses.wcnf", dirPartialWeightedMaxSATSimple, 4, 5, 2, 3, List(2, 1, 2))
  testPartialWeightedMaxSATReader("f01.wcnf", dirPartialWeightedMaxSATSimple, 3, 4, 1, 3, List(2, 1, 1))
  testPartialWeightedMaxSATReader("f02.wcnf", dirPartialWeightedMaxSATSimple, 4, 5, 2, 3, List(2, 1, 2))
  testPartialWeightedMaxSATReader("f03.wcnf", dirPartialWeightedMaxSATSimple, 3, 8, 2, 6, List(2, 1, 2, 6, 2, 1))
  testPartialWeightedMaxSATReader("f04.wcnf", dirPartialWeightedMaxSATSimple, 6, 17, 6, 11, List(7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
  testPartialWeightedMaxSATReader("f05.wcnf", dirPartialWeightedMaxSATSimple, 4, 10, 3, 7, List(2, 2, 1, 8, 2, 2, 4))
  testPartialWeightedMaxSATReader("f06.wcnf", dirPartialWeightedMaxSATSimple, 3, 4, 1, 3, List(23, 57, 1))
  testPartialWeightedMaxSATReader("f07.wcnf", dirPartialWeightedMaxSATSimple, 3, 7, 3, 4, List(5, 2, 1, 3))
  testPartialWeightedMaxSATReader("f08.wcnf", dirPartialWeightedMaxSATSimple, 5, 10, 5, 5, List(6, 7, 8, 9, 10))
  testPartialWeightedMaxSATReader("f09.wcnf", dirPartialWeightedMaxSATSimple, 3, 6, 2, 4, List(5, 3, 2, 3))
  testPartialWeightedMaxSATReader("f10.wcnf", dirPartialWeightedMaxSATSimple, 3, 9, 3, 6, List(3, 2, 3, 2, 5, 3))
  testPartialWeightedMaxSATReader("f11.wcnf", dirPartialWeightedMaxSATSimple, 3, 6, 2, 4, List(2, 3, 2, 3))
  testPartialWeightedMaxSATReader("oneClauseFormulaSoft.wcnf", dirPartialWeightedMaxSATSimple, 3, 1, 0, 1, List(2))
  testPartialWeightedMaxSATReader("oneEmptyClauseSoft.wcnf", dirPartialWeightedMaxSATSimple, 0, 1, 0, 1, List(2))
  testPartialWeightedMaxSATReader("oneVariableFormula.wcnf", dirPartialWeightedMaxSATSimple, 1, 3, 0, 3, List(2, 1, 1))
  testPartialWeightedMaxSATReader("oneVariableOneClauseFormulaSoft.wcnf", dirPartialWeightedMaxSATSimple, 1, 1, 0, 1, List(3))
  testPartialWeightedMaxSATReader("threeEmptyClauses.wcnf", dirPartialWeightedMaxSATSimple, 0, 3, 1, 2, List(2, 1))

  def testPartialWeightedMaxSATReader(filename: String,
                                      directory: String,
                                      numVars: Int,
                                      numClauses: Int,
                                      numHardClauses: Int,
                                      numSoftClauses: Int,
                                      weights: List[Long]) =
    filename + " in " + directory should {
      partialWeightedMaxSATReader.read(directory + fs + filename)
      "have " + numVars + " variables" in {
        And(partialWeightedMaxSATReader.hardClauses.map(_.toFormula).union(
          partialWeightedMaxSATReader.softClauses.map(_.toFormula)): _*).vars.size must be equalTo numVars
      }
      "have " + numClauses + " clauses" in {
        (partialWeightedMaxSATReader.hardClauses.size + partialWeightedMaxSATReader.softClauses.size) must be equalTo numClauses
      }
      "have " + numHardClauses + " hard clauses" in {
        partialWeightedMaxSATReader.hardClauses.size must be equalTo numHardClauses
      }
      "have " + numSoftClauses + " soft clauses" in {
        partialWeightedMaxSATReader.softClauses.size must be equalTo numSoftClauses
      }
      "have weights " + weights.mkString("(", ", ", ")") in {
        partialWeightedMaxSATReader.weights.toList must be equalTo weights
      }
    }
}