package org.warthog.pl.decisionprocedures.satsolver.impl

import scala.collection.mutable.Map
import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import ExecutionContext.Implicits.global

import org.warthog.pl.decisionprocedures.satsolver.{Infinity, Duration, Solver}
import org.warthog.generic.formulas.{Not, Formula}
import org.warthog.pl.formulas.PL
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.core.MSJCoreProver
import org.warthog.pl.io.CNFUtil
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections.nativeType.IntVec

class MinisatJavaSolver extends Solver {

  private var prover = new MSJCoreProver()
  private val fmtovar = Map[Formula[PL], Int]()
  private val vartofm = Map[Int, Formula[PL]]()
  private var clauses: List[IntVec] = Nil
  private var marks: List[Int] = Nil
  private var lastState = 0

  override def init() = reset()

  override def reset(): Unit = {
    prover = new MSJCoreProver
    fmtovar.clear()
    vartofm.clear()
    clauses = Nil
    marks = Nil
    lastState = 0
  }

  override def add(fm: Formula[PL]): Unit = {
    /*
     * Convert clause list to List of Set of Ints, update Int->Formula
     * and Formula->Int mapping if necessary
     */
    val newClauses = CNFUtil.toList(fm) match {
      case Nil => Nil
      case l => l.map(_.map(f => {
        val (at, phase) = f match {
          case Not(ff) => (ff, false)
          case _ => (f, true)
        }
        val lit = fmtovar.getOrElseUpdate(at, {
          prover.newVar()
          val lit = fmtovar.size
          vartofm += (lit -> at)
          lit
        })
        MSJCoreProver.mkLit(lit, !phase)
      }).toSet)
    }

    /* Convert clauses the required format  */
    val solverClauses = newClauses map {
      c =>
        val clause = new IntVec()
        c.foreach(clause.push(_))
        clause
    }

    /* add clauses to solver */
    solverClauses.foreach(prover.newClause(_, false))

    /* add clauses to solver stack */
    clauses = solverClauses ++ clauses

    /* an unsatisfiable formula doesn't get satisfiable by adding clauses */
    if (lastState > 0)
      lastState = 0
  }

  override def mark(): Unit = {
    marks = clauses.length :: marks
  }

  override def undo(): Unit = {
    marks match {
      case h :: t => {
        marks = t
        prover = new MSJCoreProver
        clauses = clauses.drop(clauses.length - h)
        clauses.foreach(prover.newClause(_, false))
      }
      case _ => {
        marks = 0 :: marks
        undo()
      }
    }
    lastState = 0
  }

  override def sat(timeout: Duration): Int =
    if (lastState != 0)
      lastState
    else
      timeout match {
        case Infinity => if (prover.solve()) 1 else -1
        case _ =>
          val f: Future[Boolean] = future {
            prover.solve()
          }
          lastState = try {
            Await.result(f, timeout.to.micros) match {
              case true => 1
              case false => -1
            }
          } catch {
            case e: TimeoutException => 0
          }
          lastState
      }

  override def getModel(): Formula[PL] = throw new NotImplementedError("Not yet implemented in MSJCoreProver")

}
