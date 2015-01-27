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

package org.warthog.pl.optimization.maxsat.partialWeighted

import org.warthog.pl.decisionprocedures.satsolver.Solver
import org.warthog.pl.generators.pbc.PBCtoSAT
import org.warthog.generic.datastructures.cnf.ClauseLike
import org.warthog.pl.formulas.{PLAtom, PL}
import org.warthog.pl.datastructures.cnf.{ImmutablePLClause, MutablePLClause, PLLiteral}
import scala.collection.mutable._
import scala.collection._
import scala.collection.Traversable
import org.warthog.pl.generators.cardinality.SortingBasedCC

/**
 * WPM1 solver.
 *
 * Reference: Solving (Weighted) Partial MaxSAT through Satisfiability Testing, Ansótegui et al, SAT, 2009
 */
class WPM1(satSolver: Solver) extends PartialWeightedMaxSATSolver() {

  var softUnsatCore: ListBuffer[Int] = null
  var idCounter = 0

  override def name = "WPM1"

  override def reset() {
    super.reset()
    softUnsatCore = null
    idCounter = 0
    satSolver.reset()
  }

  override def addHardConstraint(clause: ClauseLike[PL, PLLiteral]) {
    satSolver.add(clause)
  }

  override def markHardConstraints() {
    satSolver.mark()
  }

  def undoHardConstraints() {
    satSolver.undo()
  }

  override protected def solveMinUNSATImpl(softClauses: Traversable[ClauseLike[PL, PLLiteral]], weights: List[Long]): Long = {
    val id2WeightedClause = getId2WeightedClauseMapping(softClauses.toList, weights)
    var cost = 0L
    var loopCount = 0
    var cc = ListBuffer[ImmutablePLClause]()
    while (true) {

      val st = isSAT(id2WeightedClause, cc)

      if (st)
        return cost

      val BV = ListBuffer[PLAtom]()
      val wMin: Long = softUnsatCore.map(id => id2WeightedClause(id)._2).min
      for (id <- softUnsatCore) {
        val newBV = PLAtom(WPM1.BLOCKING_VARIABLE_PREFIX + loopCount + "_" + BV.size)

        val oldClause = id2WeightedClause(id)._1
        val oldWeight = id2WeightedClause(id)._2
        id2WeightedClause.remove(id)

        // Is weight greater than wMin?
        if (oldWeight - wMin > 0)
          id2WeightedClause(getNextID()) = (new MutablePLClause(oldClause.literals), oldWeight - wMin)

        oldClause.push(PLLiteral(newBV, true))
        id2WeightedClause(getNextID()) = (oldClause, wMin)

        BV += newBV
      }

      cc ++= getEQ1(BV)
      cost += wMin
      loopCount += 1
    }
    throw new IllegalStateException("Should never leave the loop until instance is SAT")
  }

  private def getId2WeightedClauseMapping(softClauses: List[ClauseLike[PL, PLLiteral]], weights: List[Long])
  : mutable.HashMap[Int, (MutablePLClause, Long)] = {
    val id2WeightedClause = new mutable.HashMap[Int, (MutablePLClause, Long)]()
    for (i <- 0 until softClauses.size)
      id2WeightedClause(getNextID()) = (new MutablePLClause(softClauses(i).literals), weights(i))
    id2WeightedClause
  }

  private def getNextID() = {
    val nextID = idCounter
    idCounter += 1
    nextID
  }

  private def isSAT(id2WeightedClause: mutable.HashMap[Int, (MutablePLClause, Long)],
                    cc: ListBuffer[ImmutablePLClause]): Boolean = {
    satSolver.mark()
    id2WeightedClause.values.foreach(weightedClause => satSolver.add(weightedClause._1))
    cc.foreach(c => satSolver.add(c))
    val isSAT = satSolver.sat() == Solver.SAT
    model = satSolver.getModel()
    satSolver.undo()
    if (!isSAT)
      softUnsatCore = computeSoftUnsatCore(id2WeightedClause, cc)
    isSAT
  }

  private def computeSoftUnsatCore(id2WeightedClause: mutable.HashMap[Int, (MutablePLClause, Long)],
                                   cc: ListBuffer[ImmutablePLClause]): ListBuffer[Int] = {
    val softUnsatCore = ListBuffer[Int]()

    satSolver.mark()
    cc.foreach(c => satSolver.add(c))
    // Add all soft clauses with marking
    val ids = id2WeightedClause.keySet.toArray
    for (i <- 0 until ids.length) {
      val id = ids(i)
      satSolver.mark()
      satSolver.add(id2WeightedClause(id)._1)
    }
    // Check which clause belongs to the MUS
    for (i <- ids.length - 1 to 0 by -1) {
      val id = ids(i)
      satSolver.undo()
      satSolver.mark()
      softUnsatCore.foreach(musID =>
        satSolver.add(id2WeightedClause(musID)._1))
      if (satSolver.sat() == Solver.SAT)
        softUnsatCore += id
      satSolver.undo()
    }
    satSolver.undo()
    softUnsatCore
  }

  private def getEQ1(atoms: ListBuffer[PLAtom]): ListBuffer[ImmutablePLClause] = {
    val result = new ListBuffer[ImmutablePLClause]()
    result += new ImmutablePLClause(atoms.map(a => PLLiteral(a, true)): _*)
    for (i <- 0 until atoms.size)
      for (j <- i + 1 until atoms.size)
        result += new ImmutablePLClause(PLLiteral(atoms(i), false), PLLiteral(atoms(j), false))
    result
  }

  override protected def areHardConstraintsSatisfiable() = {
    satSolver.sat() == Solver.SAT
  }
}

object WPM1 {
  val BLOCKING_VARIABLE_PREFIX = "BV___"
}
