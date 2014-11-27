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

package org.warthog.pl.decisionprocedures.satsolver.impl.picosat

import scala.collection.mutable.Map

import org.warthog.pl.decisionprocedures.satsolver.{Model, Infinity, Duration, Solver}
import org.warthog.pl.io.CNFUtil
import org.warthog.pl.formulas.PL
import org.warthog.generic.formulas._

/**
 * Solver Wrapper for Picosat
 */
class Picosat extends Solver {
  private val PSSAT = 10
  private val PSUNSAT = 20
  private val PSUNKNOWN = 0
  private val jPicosatInstance = new JPicosat()
  private var isInitialized = false
  private val fmToVar = Map[Formula[PL], Int]()
  private val varToFm = Map[Int, Formula[PL]]()
  private var clauses: List[Set[Int]] = Nil
  private var marks: List[Int] = Nil
  private var lastState = PSUNKNOWN

  override def init() {
    jPicosatInstance.picosat_init()
    isInitialized = true
  }

  override def reset() {
    jPicosatInstance.picosat_reset()
    fmToVar.clear()
    varToFm.clear()
    clauses = Nil
    marks = Nil
    lastState = PSUNKNOWN
    isInitialized = false
  }

  override def add(fm: Formula[PL]) {
    require(isInitialized, "add(): Solver not yet initialized!")
    /*
     * Convert clause list to List of Set of Ints, update Int->Formula
     * and Formula->Int mapping if necessary
     */
    val lcls = CNFUtil.toList(fm) match {
      case Nil => Nil
      case l => l.map(_.map(f => {
        val (at, mul) = f match {
          case Not(ff) => (ff, -1)
          case _ => (f, 1)
        }
        fmToVar.getOrElseUpdate(at, {
          val lit = fmToVar.size + 1
          varToFm += (lit -> at)
          lit
        }) * mul
      }).toSet)
    }
    /* add clauses to solver */
    lcls.foreach(addClauses)

    /* add clauses to solver stack */
    clauses = lcls ++ clauses

    /* an unsatisfiable formula doesn't get satisfiable by adding clauses */
    if (lastState != PSUNSAT)
      lastState = PSUNKNOWN
  }

  private def addClauses(cs: Set[Int]): Int = {
    cs.foreach(jPicosatInstance.picosat_add(_))
    jPicosatInstance.picosat_add(0)
  }

  override def sat(to: Duration): Int = {
    require(isInitialized, "sat(): Solver not yet initialized!")
    if (lastState == PSUNKNOWN) {
      /* call sat only if solver is in unknown state */
      lastState = to match {
        case Infinity => jPicosatInstance.picosat_sat(-1)
        case _ => jPicosatInstance.picosat_sat(to.to.toInt)
      }
    }
    if (lastState == PSSAT) 1 else if (lastState == PSUNSAT) -1 else 0
  }

  override def mark() {
    require(isInitialized, "mark(): Solver not yet initialized!")
    marks = clauses.length :: marks
  }

  override def undo() {
    require(isInitialized, "undo(): Solver not yet initialized!")
    marks match {
      case h :: t => {
        marks = t
        jPicosatInstance.picosat_reset()
        jPicosatInstance.picosat_init()
        clauses = clauses.drop(clauses.length - h)
        clauses.foreach(addClauses)
      }
      case _ => {
        marks = 0 :: marks
        undo()
      }
    }
    lastState = PSUNKNOWN
  }

  override def getModel(): Option[Model] = {
    require(isInitialized, "getModel(): Solver not yet initialized!")
    require(lastState == PSSAT || lastState == PSUNSAT, "getModel(): Solver needs to be in SAT or UNSAT state!")

    lastState match {
      case PSUNSAT => None
      case PSSAT => {
        val picosatLiterals = (for {
          i <- 1 to jPicosatInstance.picosat_variables()
          j = i * jPicosatInstance.picosat_deref(i)
          if j != 0 /* filter out unassigned variables */
        } yield j)
        val positiveVariables = picosatLiterals.filter(picosatLit => picosatLit > 0)
          .filter(picosatLit => varToFm.contains(picosatLit))
          .map(picosatLit => varToFm(picosatLit)).toList
        val negativeVariables = picosatLiterals.filter(picosatLit => picosatLit < 0)
          .filter(picosatLit => varToFm.contains(picosatLit))
          .map(picosatLit => varToFm(picosatLit)).toList
        Some(Model(positiveVariables, negativeVariables))
      }
    }
  }
}
