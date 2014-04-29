package org.warthog.pl.knowledgecompilation.dnnf;

import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections.IVec;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections.Vec;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections.nativeType.BooleanVec;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections.nativeType.IntVec;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.core.MSJCoreProver;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.datastructures.LBool;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.datastructures.MSJClause;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.datastructures.MSJVariable;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * @author Steffen Hildebrandt
 */
public class DNNFOperations extends MSJCoreProver {
  protected int assertionLevel = -1;
  protected IntVec lastLearnt = null;
  private boolean newlyImpliedDirty = true;
  private IVec<IntVec> compilerClauses = new Vec<IntVec>(); // all existing clauses (including Unit and Binary clauses)
  private BooleanVec subsumedClauses = new BooleanVec();
  private IVec<IntVec> varToClauses = new Vec<IntVec>(); // varToClauses(i) = List of clauses containing variable i

  public DNNFOperations() {
    super();
  }

  public LBool valueOfVar(int var) {
    return vars.get(var).assignment();
  }

  public LBool valueOfLit(int lit) {
    return value(lit);
  }

  public List<Integer> getUnsubsumedClauses(List<Integer> clauses) {
    List<Integer> unsubsumed = new LinkedList<Integer>();
    for (int i : clauses)
      if (!subsumedClauses.get(i))
        unsubsumed.add(i);
    return unsubsumed;
  }

  public boolean isClauseSubsumed(int clause) {
    return subsumedClauses.get(clause);
  }

  @Override
  public void newClause(IntVec clauseVec, boolean learnt) {
    if (!ok) {
      return;
    }
    if (clauseVec.size() == 0) {
      ok = false;
    } else if (clauseVec.size() == 1) {
      if (!enqueue(clauseVec.get(0), null)) {
        ok = false;
      }
    } else if (clauseVec.size() == 2) {
      addBinaryClause(clauseVec, learnt);
    } else {
      addNAryClause(clauseVec, learnt);
    }
  }

  protected void handleConflict(MSJClause conflict) {
    if (decisionLevel() > 0) {
      IntVec learntClause = new IntVec();
      assertionLevel = analyze(conflict, learntClause);
      lastLearnt = learntClause;
    } else {
      // solver unsat
      analyzeFinal(conflict, false);
      lastLearnt = null;
      assertionLevel = -1;
    }
  }

  /*
   * citation from "New advances in Compiling CNF to Decomposable Negation
   * Normal Form":
   *
   *   decide(l) will set literal l to true and mark the variable of l as a
   *   decision variable, and assign it a decision level: a number which is
   *   incremented each time a new decision is made. decide(l) will then
   *   apply unit resolution which would potentially imply other literals.
   *   decide(l) succeeds if no contradiction is discovered by unit
   *   resolution, otherwise, it will fail after having constructed a
   *   conflict-driven clause as descriped in [zChaff-Paper]. The mentioned
   *   method will construct a conflict-driven clause which is also an
   *   asserting, in the sense that adding this clause to the knowledge base
   *   will lead to implying the negation of literal l, -l, which is known as
   *   conflict-driven assertion. Another side effect of a failing decide(l)
   *   is to compute the assertion level, which is the second largest level
   *   for any literal in the conflict driven clause.
   */
  public boolean decide(int lit) {
    newlyImpliedDirty = true;
    if (!assume(lit)) {
      throw new AssertionError("Should not return false.");
    }
    MSJClause conflict = propagate();
    if (conflict != null) {
      handleConflict(conflict);
      return false;
    } else {
      return true;
    }
  }

  /*
   * citation from "New advances in Compiling CNF to Decomposable Negation
   * Normal Form":
   *
   *   undo-decide(l) will erase the decision level l and all other literals
   *   that were derived by unit resolution after having asserted l. The
   *   current decision level will also be decremented.
   */
  public void undoDecide(int lit) {
    newlyImpliedDirty = false;
    int backtrackLevel = v(lit).level() - 1;

    // remember affectedLiterals before backtracking
    IntVec affectedLiterals = new IntVec();
    for (int c = trail.size() - 1; c >= trailLimits.get(backtrackLevel); c--)
      affectedLiterals.push(trail.get(c));

    cancelUntil(backtrackLevel); // backtracking

    // restore validity of subsumedClauses
    if (decisionLevel() > backtrackLevel) {
      for (int n = 0; n < affectedLiterals.size(); n++) {
        int btlit = affectedLiterals.get(n);
        int btvar = var(btlit);
        IntVec affectedClauses = varToClauses.get(btvar);
        for (int i = 0; i < affectedClauses.size(); i++) {
          IntVec clause = compilerClauses.get(affectedClauses.get(i));
          if (subsumedClauses.get(i) || clause.contains(btlit)) {
            // re-check all literals
            subsumedClauses.set(i, checkSubsumed(clause));
          }
        }
      }
    }
  }

  private boolean checkSubsumed(IntVec clause) {
    for (int i = 0; i < clause.size(); i++) {
      if (valueOfLit(clause.get(i)) == LBool.TRUE)
        return true;
    }
    return false;
  }

  /*
   * citation from "New advances in Compiling CNF to Decomposable Negation
   * Normal Form":
   *
   *   at-assertion-level() is a predicate that succeeds if the current
   *   decision level equals the assertion level computed by the last call to
   *   decide(l).
   */
  public boolean atAssertionLevel() {
    return decisionLevel() == assertionLevel;
  }

  /*
   * citation from "New advances in Compiling CNF to Decomposable Negation
   * Normal Form":
   *
   *   assert-cd-literal() will add the conflict-driven clause constructed
   *   by the last call to decide(l). This will in turn lead to implying
   *   -l, the conflict-driven assertion. Unit resolution will then be
   *   applied which would potentially imply new literals. This may also
   *   lead to discovering a contradiction in which case assert-cd-literal()
   *   will fail, after having constructed a conflict-driven clause and
   *   computed a new assertion level (just like a call to decide()).
   */
  public boolean assertCdLiteral() {
    newlyImpliedDirty = true;
    if (!atAssertionLevel())
      throw new RuntimeException("assertCdLiteral called although not at assertion level!");

    int propLit = lastLearnt.get(0);

    newClause(lastLearnt, true);
    if (!ok)
      return false;

    if (lastLearnt.size() > 2) {
      if (!enqueue(propLit, learnts.last()))
        throw new RuntimeException("What's this supposed to mean?");
    } else {
      // propLit was already enqueued by newClause
    }

    MSJClause confl = propagate();
    if (confl != null) {
      handleConflict(confl);
      return false;
    }
    return true;
  }

  /*
   * newly implied literals, i.e. all literals on the trail until (but not
   * including) last decision
   */
  public List<Integer> newlyImplied() {
    List<Integer> rv = new ArrayList<Integer>();
    if (newlyImpliedDirty) {
      int limit = trailLimits.isEmpty() ? -1 : trailLimits.last();
      for (int i = trail.size() - 1; i > limit; i--)
        rv.add(trail.get(i));
    }
    newlyImpliedDirty = false;
    return rv;
  }

  @Override
  public MSJClause propagate() {
    newlyImpliedDirty = true;
    return super.propagate();
  }

  @Override
  protected boolean enqueue(int lit, MSJClause reason) {
    IntVec affectedClauses = varToClauses.get(var(lit));
    for (int i = 0; i < affectedClauses.size(); i++) {
      if (!subsumedClauses.get(i)) {
        IntVec clause = compilerClauses.get(affectedClauses.get(i));
        if (clause.contains(lit))
          subsumedClauses.set(i, true);
      }
    }
    return super.enqueue(lit, reason);
  }

  /**
   * Sets up the solver, returns true if it worked.
   * If the given clause set is unsatisfiable by unit propagation, it returns false.
   * @param clauses The clause set for the DNNF
   * @return true, if initialization worked, false otherwise
   */
  public boolean initSolver(List<Set<Integer>> clauses) {
    int maxVar = -1;
    for (int i = 0; i < clauses.size(); i++) {
      Set<Integer> clause = clauses.get(i);
      IntVec solverClause = new IntVec();
      for (Integer lit : clause) {
        int v = var(lit);
        while (maxVar < v) {
          newVar();
          maxVar++;
          varToClauses.push(new IntVec());
        }
        varToClauses.get(v).push(i); // add new clause to list of clauses for variable v
        solverClause.push(lit);
      }
      compilerClauses.push(solverClause);
      subsumedClauses.push(false);
      newClause(solverClause, false);
    }
    return propagate() == null;
  }

  /**
   * A recursive SAT solving method as proposed by Darwiche.
   * The primary use is to test the functionality of this class.
   *
   * @return True, if the solver state is satisfiable, false otherwise
   */
  public boolean recursiveSolve() {
    if (!ok)
      return false;
    int i = 0;
    while (i < vars.size() && vars.get(i).assignment() != LBool.UNDEF) i++;
    if (i >= vars.size())
      return true;
    int lit = mkLit(vars.get(i).num(), false);
    if (decide(lit) && recursiveSolve())
      return true;
    undoDecide(lit);
    if (atAssertionLevel())
      return assertCdLiteral() && recursiveSolve();
    return false;
  }
}
