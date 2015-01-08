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

package org.warthog.pl.optimization.maxsat.partialweighted

import org.specs2.mutable.Specification
import java.io.File
import org.warthog.pl.optimization.maxsat.partialWeighted.{BinarySearch, PartialWeightedMaxSATSolver}
import org.warthog.pl.parsers.maxsat.PartialWeightedMaxSATReader
import org.warthog.pl.decisionprocedures.satsolver.Solver
import org.warthog.pl.decisionprocedures.satsolver.impl.picosat.Picosat
import org.warthog.pl.generators.pbc.{BailleuxBoufkhadRoussel, BailleuxBoufkhadRousselTest}

class BinarySearchTest extends Specification {
  /*
   * By default, tests are executed concurrently. JNI/JNA, however, is able to load _only one_ instance of
   * (lib)picosat.{so,dylib,dll} per JVM so concurrently accessing the picosat INSTANCE will result in double
   * instantiation errors and unexpected behaviour.
   */
  args(sequential = true)

  val fs = System.getProperty("file.separator")
  val solver = new BinarySearch(new Picosat(), BailleuxBoufkhadRoussel)

  private def getFileString(folder: String, subFolder: String, file: String) =
    List("src", "test", "resources", folder, subFolder, file).mkString(File.separator)

  private def testWCNFDIMACSFile(subFolder: String, fileName: String, solver: PartialWeightedMaxSATSolver, expResult: Option[Long]) {
    val expText = if (expResult.isEmpty) "no solution" else "solution " + expResult.get
    "File " + fileName should {
      "have " + expText in {
        var reader = new PartialWeightedMaxSATReader()
        reader.read(getFileString("maxsat", subFolder, fileName))

        solver.reset()
        solver.addHardConstraint(reader.hardClauses)
        var result = solver.solveMinUNSAT(reader.softClauses, reader.weights.toList)

        result must be equalTo expResult
      }
    }
  }

  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "emptyAndNotEMptyClauses.wcnf", solver, None)

  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "f01.wcnf", solver, Some(0))
  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "f02.wcnf", solver, Some(0))
  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "f03.wcnf", solver, Some(1))
  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "f04.wcnf", solver, Some(8))
  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "f05.wcnf", solver, Some(4))

  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "f06.wcnf", solver, Some(1))
  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "f07.wcnf", solver, Some(6))
  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "f08.wcnf", solver, Some(40))
  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "f09.wcnf", solver, None)
  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "f10.wcnf", solver, None)

  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "f11.wcnf", solver, Some(4))

  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "oneClauseFormulaSoft.wcnf", solver, Some(0))
  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "oneEmptyClauseSoft.wcnf", solver, Some(2))
  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "oneVariableFormula.wcnf", solver, Some(0))
  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "oneVariableOneClauseFormulaSoft.wcnf", solver, Some(0))
  testWCNFDIMACSFile("partialWeighted" + fs + "simple", "threeEmptyClauses.wcnf", solver, None)

  testWCNFDIMACSFile("partial" + fs + "randomVertexCover", "edges00040_vertices00010.wcnf", solver, Some(8))
  testWCNFDIMACSFile("partial" + fs + "randomVertexCover", "edges00150_vertices00020.wcnf", solver, Some(16))
}
