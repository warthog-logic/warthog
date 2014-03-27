package org.warthog.pl.knowledgecompilation.dnnf;

import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections.nativeType.IntVec;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.core.MSJCoreProver;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.datastructures.LBool;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.datastructures.MSJClause;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Steffen Hildebrandt
 */
public class DNNFOperations extends MSJCoreProver {
  protected int assertionLevel = -1;
  protected IntVec lastLearnt = null;

  public DNNFOperations() {
    super();
  }

  public LBool valueOfVar(int var) {
    return vars.get(var).assignment();
  }

  public LBool valueOfLit(int lit) {
    return value(lit);
  }

  protected void handleConflict(MSJClause conflict) {
    IntVec learntClause = new IntVec();
    int backtrackLevel = analyze(conflict, learntClause);
        /*
        // compute new assertionLevel
        int highestLevel = learntClause.get(0);
        int sndHighestLevel = 0;
        for (int i = 1; i < learntClause.size(); i++) {
            int level = learntClause.get(i);
            if (level > highestLevel) {
              sndHighestLevel = highestLevel;
              highestLevel = level;
            } else if (level > sndHighestLevel) {
              sndHighestLevel = level;
            }
        }
        assertionLevel = sndHighestLevel;
        */
    lastLearnt = learntClause;
    assertionLevel = backtrackLevel;

    //      LearntClause learnt=new LearntClause(this);
    //      int i=trail.size()-1,
    //              n=0,
    //              lit=0;
    //      Object reason=conflict_reason;
    //
    //      do {
    //        if (reason==null) /* decision */
    //          break;
    //        if (reason instanceof Integer) {/* binary clause */
    //          if (lit==0 && !seen.get(lit2var(conflict_lit))) {
    //            seen.set(lit2var(conflict_lit), true);
    //            if (level<=getLevel(conflict_lit))
    //              n++;
    //            else { /* may not happen... */
    //              learnt.push(conflict_lit);
    //            }
    //          }
    //          if (!seen.get(lit2var((Integer)reason))) {
    //            seen.set(lit2var((Integer)reason), true);
    //            if (level<=getLevel((Integer)reason))
    //              n++;
    //            else {
    //              learnt.push((Integer)reason);
    //            }
    //          }
    //        } else {
    //          Clause cls=(Clause)reason;
    //          for (int j=(lit==0 ? 0 : 1); j<cls.size(); j++) {
    //                    /*
    //                          * UIP is reached, if all but one literals of the current level are resolved, i.e. if a
    //                          * literal at or above the current level is encountered, increase counter,
    // else push it to
    //                          * the learnt clause
    //                          */
    //            int _lit=cls.get(j);
    //            if (!seen.get(lit2var(_lit))) {
    //              seen.set(lit2var(_lit), true);
    //              if (level<=getLevel(_lit))
    //                n++;
    //              else {
    //                learnt.push(_lit);
    //              }
    //            }
    //            if (!glucose_clause_scores && cls.isLearnt())
    //              ((LearntClause)cls).increaseActivity();
    //          }
    //        }
    //            /*
    //                * jump to last assigned literal on trail which contributes to conflict
    //                * (i.e. takes part in conflict clause resolution)
    //                */
    //        //System.out.println(trail);
    //        while (!seen.get(lit2var(trail.get(i--))))
    //          ;
    //        lit=trail.get(i+1);
    //        seen.set(lit2var(lit), false);
    //        reason=lit2variable(lit).reason();
    //        n--; /* literal is resolved, thus decrease counter */
    //      } while (n > 0);
    //      learnt.push(oppositeLit(lit));
    //      learnt.swap(0, learnt.size()-1); /* uip at position 0 */
    //
    //        /* put literal with second largest decision level at position 1 */
    //      int snd_pos=1;
    //      int bt_level;
    //      permdiff_curr++;
    //      int lbd=1;
    //      for (i=1; i<learnt.size(); i++) {
    //        int __lit=learnt.get(i);
    //        int _level=getLevel(__lit);
    //
    //        if (_level>getLevel(learnt.get(snd_pos)))
    //          snd_pos=i;
    //        if (glucose_clause_scores && perm_diff.get(_level)!=permdiff_curr) {
    //          perm_diff.set(_level, permdiff_curr);
    //          lbd++;
    //        }
    //
    //        seen.set(lit2var(__lit), false);
    //        lit2variable(__lit).incScore();
    //      }
    //      if (glucose_clause_scores) /* set glucose-style clause activity */
    //        learnt.setActivity(lbd);
    //
    //      seen.set(lit2var(learnt.get(0)), false);
    //      lit2variable(learnt.get(0)).incScore();
    //      learnt.swap(1, snd_pos);
    //
    //      assertionLevel=(learnt.size()<=1 ? 0 : getLevel(learnt.get(1)));
    //      lastLearnt=learnt;
    //
    //        /* add clause, backtrack, propagate uip, restart if threshold is met */
    //      stats.statConflict();

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
    //        newlyImpliedDirty = true;
    //        level++;
    //        assign(lit, null);
    //
    //        if (!bcp()) {
    //          handleConflict();
    //          return false;
    //        }
    //        return true;
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
    cancelUntil(v(lit).level() - 1);
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
      throw new RuntimeException("assertCdLiteral called though not at assertion level!");

    int propLit = lastLearnt.get(0);
    boolean failure = false;

    //System.out.println(lastLearnt);
    newClause(lastLearnt, true);
    if (lastLearnt.size() > 1)
      if (!enqueue(propLit, learnts.last()))
        throw new RuntimeException("What's this supposed to mean?");
    //lastLearnt = null;

    MSJClause confl = propagate();
    if (confl != null) {
      handleConflict(confl);
      return false;
    }
    return true;

    //        newlyImpliedDirty = true;
    //        if (!atAssertionLevel())
    //            throw new Exception("assertCdLiteral called though not at assertion level!");
    //
    //        int propLit = lastLearnt.get(0);
    //        boolean failure = false;
    //
    //        //System.out.println(lastLearnt);
    //        failure |= !pushClause(lastLearnt);
    //        if (lastLearnt.size()>1)
    //            failure |= !assign(propLit, (lastLearnt.size()==2 ? oppositeLit(lastLearnt.get(1)) : lastLearnt));
    //        //lastLearnt = null;
    //
    //        if (failure || !bcp()) {
    //            handleConflict();
    //            return false;
    //        }
    //        return true;
  }

  private boolean newlyImpliedDirty = true;

  /*
   * newly implied literals, i.e. all literals on the trail until (but not
   * including) last decision
   */
  public List<Integer> newlyImplied() {
    List<Integer> rv = new ArrayList<Integer>();
    if (newlyImpliedDirty) {
      for (int i = trail.size() - 1; i > trailLimits.last(); i--)
        rv.add(trail.get(i));
    }
    newlyImpliedDirty = false;
    return rv;
    //        List<Integer> rv = new ArrayList<Integer>();
    //        if (newlyImpliedDirty) {
    //            int i=trail.size()-1;
    //
    //            while (i>=0 && (lit2variable(trail.get(i)).reason()!=null || level==0))
    //                rv.add(trail.get(i--));
    //        }
    //        newlyImpliedDirty = false;
    //        return rv;
  }

  @Override
  public MSJClause propagate() {
    newlyImpliedDirty = true;
    return super.propagate();
  }

  /**
   * A recursive SAT solving method as proposed by Darwiche.
   * The primary use is to test the functionality of this class.
   *
   * @return True, if the solver state is satisfiable, false otherwise
   */
  public boolean recursiveSolve() {
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
