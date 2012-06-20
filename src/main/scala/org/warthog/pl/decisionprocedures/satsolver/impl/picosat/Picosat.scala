/**
 * Copyright (c) 2011, Andreas J. Kuebler & Christoph Zengler
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

import org.warthog.pl.decisionprocedures.satsolver.{Infinity, Duration, Solver}
import org.warthog.pl.io.CNFUtil
import org.warthog.pl.formulas.PL
import org.warthog.generic.formulas._

/**
 * Solver Wrapper for Picosat
 *
 * Author: kuebler
 * Date:   25.01.12
 */
class Picosat extends Solver {
  private val PSSAT = 10
  private val PSUNSAT = 20
  private val PSUNKNOWN = 0
  private val jps = new JPicosat()
  private var initialized = false;
  private val fmtovar = Map[Formula[PL], Int]()
  private val vartofm = Map[Int, Formula[PL]]()
  private var clss: List[Set[Int]] = Nil
  private var marks: List[Int] = Nil
  private var laststate = PSUNKNOWN

  override def init(): Unit = {
    jps.picosat_init()
    initialized = true
  }

  override def reset(): Unit = {
    jps.picosat_reset()
    fmtovar.clear()
    vartofm.clear()
    clss = Nil
    marks = Nil
    laststate = PSUNKNOWN
    initialized = false
  }

  override def add(fm: Formula[PL]): Unit = {
    require(initialized, "add(): Solver not yet initialized!")
    /*
     * Convert clause list to List of Set of Ints, update Int->Formula
     * and Formula->Int mapping if necessary
     */
    val lcls = CNFUtil.toList(fm) match {
      case Nil => Nil
      case l   => l.map(_.map(f => {
        val (at, mul) = f match {
          case Not(ff) => (ff, -1)
          case _       => (f, 1)
        }
        fmtovar.getOrElseUpdate(at, {
          val lit = fmtovar.size + 1
          vartofm += (lit -> at)
          lit
        }) * mul
      }).toSet)
    }
    /* add clauses to solver */
    lcls.foreach(add_cls)

    /* add clauses to solver stack */
    clss = lcls ++ clss

    /* an unsatisfiable formula doesn't get satisfiable by adding clauses */
    if (laststate != PSUNSAT)
      laststate = PSUNKNOWN
  }

  private def add_cls(cs: Set[Int]): Int = {
    cs.foreach(jps.picosat_add(_))
    jps.picosat_add(0)
  }

  override def sat(to: Duration): Int = {
    require(initialized, "sat(): Solver not yet initialized!")
    if (laststate == PSUNKNOWN) {
      /* call sat only if solver is in unknown state */
      laststate = to match {
        case Infinity => jps.picosat_sat(-1)
        case _        => jps.picosat_sat(to.to.toInt)
      }
    }
    if (laststate == PSSAT) 1 else if (laststate == PSUNSAT) -1 else 0
  }

  override def mark(): Unit = {
    require(initialized, "mark(): Solver not yet initialized!")
    marks = clss.length :: marks
  }

  override def undo(): Unit = {
    require(initialized, "undo(): Solver not yet initialized!")
    marks match {
      case h :: t => {
        marks = t
        jps.picosat_reset()
        jps.picosat_init()
        clss = clss.drop(clss.length - h)
        clss.foreach(add_cls)
      }
      case _      => {
        marks = 0 :: marks
        undo()
      }
    }
    laststate = PSUNKNOWN
  }

  override def getModel(): Formula[PL] = {
    require(initialized, "getModel(): Solver not yet initialized!")
    require(laststate == PSSAT || laststate == PSUNSAT, "getModel(): Solver needs to be in SAT or UNSAT state!")

    def toConjunction(a: Seq[Formula[PL]]) =
      if (a.isEmpty)
        Verum()
      else
        a.reduceLeft(And(_, _))

    laststate match {
      case PSUNSAT => Falsum()
      case PSSAT   =>
        toConjunction((for {
          i <- 1 to jps.picosat_variables()
          j = i * jps.picosat_deref(i)
          if j != 0 /* filter out unassigned variables */
        } yield j).
          map(l => /* map literals to proper generic */
          if (l < 0)
            Not(vartofm.getOrElse(-l, Falsum()))
          else
            vartofm.getOrElse(l, Verum())
        ).toSeq)
    }
  }
}
