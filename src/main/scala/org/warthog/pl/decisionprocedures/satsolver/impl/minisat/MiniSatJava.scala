/*
 * Copyright (c) 2011-2014, Andreas J. Kuebler & Christoph Zengler & Rouven Walter & Konstantin Grupp
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

package org.warthog.pl.decisionprocedures.satsolver.impl.minisat

import scala.collection.mutable.Map
import scala.collection.JavaConverters._

import org.warthog.pl.formulas.{PLAtom, PL}
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.core.MSJCoreProver
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections.nativeType.IntVec
import org.warthog.generic.datastructures.cnf.ClauseLike
import org.warthog.pl.datastructures.cnf.PLLiteral
import org.warthog.pl.decisionprocedures.satsolver.{Model, Solver}

/**
 * Solver Wrapper for MiniSatJava.
 */
class MiniSatJava extends Solver {
  private var miniSatJavaInstance = new MSJCoreProver()
  private val varToID = Map[PLAtom, Int]()
  private val idToVar = Map[Int, PLAtom]()
  private var clausesStack: List[ClauseLike[PL, PLLiteral]] = Nil
  private var marks: List[Int] = Nil
  private var lastState = Solver.UNKNOWN

  override def name = "MiniSatJava"

  override def reset() {
    miniSatJavaInstance = new MSJCoreProver
    varToID.clear()
    idToVar.clear()
    clausesStack = Nil
    marks = Nil
    lastState = Solver.UNKNOWN
  }

  override def add(clause: ClauseLike[PL, PLLiteral]) {
    clausesStack = (clause :: clausesStack)
    addClauseToSolver(clause)

    /* an unsatisfiable formula doesn't get satisfiable by adding clauses */
    if (lastState != Solver.UNSAT)
      lastState = Solver.UNKNOWN
  }

  private def addClauseToSolver(clause: ClauseLike[PL, PLLiteral]) {
    val clauseAsIntVec = new IntVec(clause.literals.map(literal => {
      val (v, phase) = (literal.variable, literal.phase)
      val id = varToID.getOrElseUpdate(v, {
        miniSatJavaInstance.newVar()
        val nextID = varToID.size
        idToVar += (nextID -> v)
        nextID
      })
      MSJCoreProver.mkLit(id, !phase)
    }).toArray)

    miniSatJavaInstance.newClause(clauseAsIntVec, false)
  }

  override def mark() {
    marks = clausesStack.length :: marks
  }

  override def undo() {
    marks match {
      case h :: t => {
        marks = t
        miniSatJavaInstance = new MSJCoreProver
        varToID.clear()
        idToVar.clear()
        clausesStack = clausesStack.drop(clausesStack.length - h)
        clausesStack.foreach(addClauseToSolver(_))
        lastState = Solver.UNKNOWN
      }
      case _ => // No mark, then ignore undo
    }
  }

  override def sat(): Int = {
    if (lastState == Solver.UNKNOWN)
    /* call sat only if solver is in unknown state */
      lastState = MiniSatJava.miniSatJavaStateToSolverState(miniSatJavaInstance.solve())
    lastState
  }

  override def getModel(): Option[Model] = {
    require(lastState == Solver.SAT || lastState == Solver.UNSAT, "getModel(): Solver needs to be in SAT or UNSAT state!")

    lastState match {
      case Solver.UNSAT => None
      case Solver.SAT => {
        val miniSatJavaModel: List[Integer] = miniSatJavaInstance.getModel().asScala.toList
        val positiveVariables = miniSatJavaModel.filter {
          lit => !MSJCoreProver.sign(lit)
        }.map {
          lit => idToVar(MSJCoreProver.`var`(lit))
        }.toList
        val negativeVariables = miniSatJavaModel.filter {
          lit => MSJCoreProver.sign(lit)
        }.map {
          lit => idToVar(MSJCoreProver.`var`(lit))
        }.toList
        Some(Model(positiveVariables, negativeVariables))
      }
    }
  }
}

object MiniSatJava {
  private def miniSatJavaStateToSolverState(miniSatJavaState: Boolean) = miniSatJavaState match {
    case false => Solver.UNSAT
    case true => Solver.SAT
  }
}