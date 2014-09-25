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

package org.warthog.pl.generators.pbc

import org.specs2.mutable.Specification
import org.warthog.pl.datastructures.cnf.{ImmutablePLClause => Clause, PLLiteral => Lit}
import org.warthog.pl.decisionprocedures.satsolver.impl.picosat.Picosat
import org.warthog.pl.decisionprocedures.satsolver.{ Infinity, sat }
import org.warthog.generic.formulas.{Or, And, Formula}
import org.specs2.mutable.Specification
import org.warthog.pl.formulas.{ PL, PLAtom }
import org.warthog.pl.generators.cardinality._

/**
 * Test for the PBCtoSAT encoding
 */
class PBCtoSATTest extends Specification {

  def toFormula(l: List[Clause]): Formula[PL] = {
    And(l.map(_.toFormula):_*)
  }

  val x1 = Lit("x_1",true)
  val x2 = Lit("x_2",true)
  val x3 = Lit("x_3",true)
  val x4 = Lit("x_4",true)
  val x1F = x1.toFormula
  val x2F = x2.toFormula
  val x3F = x3.toFormula
  val x4F = x4.toFormula

  "2x_1 + 3x_2 + 4x_3 <= 6" should {
    val pbc1 = List((2,x1),(3,x2),(4,x3))
    val sol1 = "List((D_3_6), (~D_2_2, D_3_6), (~D_3_6, D_2_6), (~D_3_6, ~x_3, D_2_2), (~D_2_6, x_3, D_3_6), (D_2_6), (~D_1_-1, D_2_2), (~D_2_2, D_1_2), (~D_2_2, ~x_2, D_1_-1), (~D_1_2, x_2, D_2_2), (D_1_2), (~D_1_-1))"
    "be equal to sol1" in {
      BailleuxBoufkhadRoussel.le(pbc1,6).toString() must be equalTo sol1
    }
  }

  "1x_1 + 2x_2 + 3x_3 + 5x_4 <= 6" should {
    val pbc2 = List((1,x1),(2,x2),(3,x3),(5,x4))
    val sol2 = "List((D_4_6), (~D_3_1, D_4_6), (~D_4_6, D_3_6), (~D_4_6, ~x_4, D_3_1), (~D_3_6, x_4, D_4_6), (D_3_6), (~D_2_-2, D_3_1), (~D_3_1, D_2_1), (~D_3_1, ~x_3, D_2_-2), (~D_2_1, x_3, D_3_1), (~D_1_-1, D_2_1), (~D_2_1, D_1_1), (~D_2_1, ~x_2, D_1_-1), (~D_1_1, x_2, D_2_1), (D_1_1), (~D_1_-1), (~D_2_-2))"
    "be equal to sol2" in {
      BailleuxBoufkhadRoussel.le(pbc2,6).toString() must be equalTo sol2
    }
  }

  "1x_1 + 2x_2 + 3x_3 + 6x_4 <= 6" should {
    val pbc3 = List((6,x4), (2,x2), (1,x1), (3,x3))
    val sol3 = "List((D_4_6), (~D_3_0, D_4_6), (~D_4_6, D_3_6), (~D_4_6, ~x_4, D_3_0), (~D_3_6, x_4, D_4_6), (D_3_6), (D_3_0, x_1, x_2, x_3), (~D_3_0, ~x_1), (~D_3_0, ~x_2), (~D_3_0, ~x_3))"
    "be equal to sol3" in {
      BailleuxBoufkhadRoussel.le(pbc3,6).toString() must be equalTo sol3
    }
  }

  val ps = new Picosat
  var rv: Int = _

  args(sequential = true)

  testSat("2x_1+3x_2+4x_3 <= 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.le(List((2,x1),(3,x2),(4,x3)), 6))
  lower("2x_1+3x_2+4x_3 <= 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.le(List((2,x1),(3,x2),(4,x3)), 6))
  lower2("2x_1+3x_2+4x_3 <= 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.le(List((2,x1),(3,x2),(4,x3)), 6))
  testSat("2x_1+3x_2+4x_3 < 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.lt(List((2,x1),(3,x2),(4,x3)), 6))
  lower("2x_1+3x_2+4x_3 < 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.lt(List((2,x1),(3,x2),(4,x3)), 6))
  lower2("2x_1+3x_2+4x_3 < 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.lt(List((2,x1),(3,x2),(4,x3)), 6))
  testSat("1x_1+2x_2+3x_3+6x_4 <= 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.le(List((1,x1),(2,x2),(3,x3),(6,x4)), 6))
  lower("1x_1+2x_2+3x_3+5x_4 <= 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.le(List((1,x1),(2,x2),(3,x3),(5,x4)), 6))
  lower2("1x_1+2x_2+3x_3+5x_4 <= 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.le(List((1,x1),(2,x2),(3,x3),(5,x4)), 6))
  testSat("1x_1+2x_2+3x_3+5x_4 < 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.lt(List((1,x1),(2,x2),(3,x3),(5,x4)), 6))
  lower("1x_1+2x_2+3x_3+5x_4 < 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.lt(List((1,x1),(2,x2),(3,x3),(5,x4)), 6))
  lower2("1x_1+2x_2+3x_3+5x_4 < 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.lt(List((1,x1),(2,x2),(3,x3),(5,x4)), 6))
  testSat("1x_1+2x_2+3x_3+6x_4 <= 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.le(List((1,x1),(2,x2),(3,x3),(6,x4)), 6))
  lower("1x_1+2x_2+3x_3+6x_4 <= 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.le(List((1,x1),(2,x2),(3,x3),(6,x4)), 6))
  lower2("1x_1+2x_2+3x_3+6x_4 <= 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.le(List((1,x1),(2,x2),(3,x3),(6,x4)), 6))
  testSat("2x_1+3x_2+4x_3 <= 10 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.le(List((2,x1),(3,x2),(4,x3)), 10))
  lower2("2x_1+3x_2+4x_3 <= 10 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.le(List((2,x1),(3,x2),(4,x3)), 10))
  testSat("2x_1+3x_2+4x_3 <= 9 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.le(List((2,x1),(3,x2),(4,x3)), 9))
  lower2("2x_1+3x_2+4x_3 <= 9 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.le(List((2,x1),(3,x2),(4,x3)), 9))
  testSat("x_1+2x_2 <= 0 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.le(List((1,x1),(2,x2)), 0))
  testUnsat("x_1+2x_2 < 0 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.lt(List((1,x1),(2,x2)), 0))
  testUnsat("x_1+2x_2 <= -1 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.le(List((1,x1),(2,x2)), -1))

  testSat("2x_1+3x_2+4x_3 >= 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.ge(List((2,x1),(3,x2),(4,x3)), 6))
  greater("2x_1+3x_2+4x_3 >= 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.ge(List((2,x1),(3,x2),(4,x3)), 6))
  testSat("2x_1+3x_2+4x_3 > 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.gt(List((2,x1),(3,x2),(4,x3)), 6))
  greater("2x_1+3x_2+4x_3 > 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.gt(List((2,x1),(3,x2),(4,x3)), 6))
  testSat("2x_1+3x_2+4x_3 > 5 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.gt(List((2,x1),(3,x2),(4,x3)), 5))
  greater("2x_1+3x_2+4x_3 > 5 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.gt(List((2,x1),(3,x2),(4,x3)), 5))
  testSat("11x_1+9x_2+8x_3 >= 28 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.ge(List((11,x1),(9,x2),(8,x3)), 28))
  testUnsat("11x_1+9x_2+8x_3 > 28 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.gt(List((11,x1),(9,x2),(8,x3)), 28))

  testSat("2x_1+3x_2+4x_3+x_8 == 5 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.eq(List((2,x1),(3,x2),(4,x3),(8,x4)), 5))
  equal("2x_1+3x_2+4x_3+x_8 == 5 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.eq(List((2,x1),(3,x2),(4,x3),(8,x4)), 5))
  testSat("10x_1+1x_2+4x_3+x_8 == 11 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.eq(List((10,x1),(1,x2),(4,x3),(8,x4)), 11))
  equal("10x_1+1x_2+4x_3+x_8 == 11 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.eq(List((10,x1),(1,x2),(4,x3),(8,x4)), 11))
  testSat("2x_1+4x_2+3x_3 == 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.eq(List((2,x1),(4,x2),(3,x3)), 6))
  equal("2x_1+4x_2+3x_3 == 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.eq(List((2,x1),(4,x2),(3,x3)), 6))
  testUnsat("1x_1+4x_2+3x_3 == 6 (Bailleux-Boufkhad-Roussel)", BailleuxBoufkhadRoussel.eq(List((1,x1),(4,x2),(3,x3)), 6))

  def testSat(name: String, clauses: List[Clause]) = name should {
    "be satisfiable" in {
      sat(ps) {
        s => {
          s.add(toFormula(clauses))
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo (1)
    }
  }

  def testUnsat(name: String, clauses: List[Clause]) = name should {
    "be unsatisfiable" in {
      sat(ps) {
        s => {
          s.add(toFormula(clauses))
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo (-1)
    }
  }

  def lower(name: String, clauses: List[Clause]) = name should {
    "be unsatisfiable after adding x_2=x_3=x_4=true" in {
      sat(ps) {
        s =>
        {
          s.add(toFormula(clauses))
          s.add(x2F && x3F && x4F)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo (-1)
    }
  }

  def lower2(name: String, clauses: List[Clause]) = name should {
    "be satisfiable for x_1=x_2=x_3=false x_4=true" in {
      sat(ps) {
        s =>
        {
          s.add(toFormula(clauses))
          s.add(-x1F && -x2F && -x3F && x4F)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo (1)
    }
  }

  def greater(name: String, clauses: List[Clause]) = name should {
    "be unsatisfiable after adding x_3=false" in {
      sat(ps) {
        s =>
        {
          s.add(toFormula(clauses))
          s.add(-x3F)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo (-1)
    }
    "be satisfiable for x_1=x_2=x_3=true" in {
      sat(ps) {
        s =>
        {
          s.add(toFormula(clauses))
          s.add(x1F && x2F && x3F)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo (1)
    }
  }

  def equal(name: String, clauses: List[Clause]) = name should {
    "be unsatisfiable after adding x_1=false" in {
      sat(ps) {
        s =>
        {
          s.add(toFormula(clauses))
          s.add(-x1F)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo (-1)
    }
    "be satisfiable for x_1=x_2=true x_3=x_4=false" in {
      sat(ps) {
        s =>
        {
          s.add(toFormula(clauses))
          s.add(x1F && x2F && -x3F && -x4F)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo (1)
    }
  }

}

