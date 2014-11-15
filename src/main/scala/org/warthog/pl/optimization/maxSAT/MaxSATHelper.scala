package org.warthog.pl.optimization.maxSAT

import org.warthog.pl.datastructures.cnf.ImmutablePLClause
import org.warthog.pl.decisionprocedures.satsolver.impl.picosat.Picosat
import org.warthog.pl.decisionprocedures.satsolver.Infinity
import collection.mutable

object MaxSATHelper {
  def isSAT(clauses: mutable.Traversable[ImmutablePLClause]) = {
    val satSolver = new Picosat
    clauses.foreach(c => satSolver.add(c.toFormula))
    satSolver.sat(Infinity) > 0
  }
}
