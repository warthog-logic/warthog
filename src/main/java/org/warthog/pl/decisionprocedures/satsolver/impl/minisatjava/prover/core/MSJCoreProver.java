/******************************************************************************************
 MiniSat -- Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson

 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 associated documentation files (the "Software"), to deal in the Software without restriction,
 including without limitation the rights to use, copy, modify, merge, publish, distribute,
 sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all copies or
 substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 **************************************************************************************************
 Remarks:
 * The following source code is basically a Java conversion of the C/C++ MiniSAT v1.14
 **************************************************************************************************/

package org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.core;

import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections.HeapWithIndex;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections.IVec;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections.Vec;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections.nativeType.BooleanVec;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections.nativeType.IntVec;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.core.stats.SearchParams;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.core.stats.SolverStats;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.datastructures.LBool;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.datastructures.MSJClause;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.datastructures.MSJVariable;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * MiniSAT Java
 */
public class MSJCoreProver {
    //////////////////////
    // The solver state //
    //////////////////////
    protected boolean ok = true;
    protected IVec<MSJClause> clauses = new Vec<MSJClause>();
    protected IVec<MSJClause> learnts = new Vec<MSJClause>();
    protected IVec<MSJVariable> vars = new Vec<MSJVariable>();
    private HeapWithIndex<MSJVariable> varHeap = new HeapWithIndex<MSJVariable>();
    private IVec<IVec<MSJClause>> watches = new Vec<IVec<MSJClause>>();
    protected IntVec trail = new IntVec();
    protected IntVec trailLimits = new IntVec();
    private int rootLevel;
    private int qhead = 0;
    protected BooleanVec seen = new BooleanVec();
    public SolverStats stats = new SolverStats();
    private SearchParams params = new SearchParams();
    private BooleanVec model = new BooleanVec();
    protected IntVec conflict = new IntVec();
    double learntsize_adjust_confl;
    int learntsize_adjust_cnt;
    double learntsize_factor = 1.0 / 3.0;
    double learntsize_inc = 1.1;
    int learntsize_adjust_start_confl = 100;
    double learntsize_adjust_inc = 1.5;
    int restart_first = 100;
    double restart_inc = 2.0;
    double max_learnts;
    ////////////////////////////////
    // Literal & variable helpers //
    ////////////////////////////////
    private final static int litUndef = -1;

    public static int mkLit(int var, boolean sign) {
        return var + var + (sign ? 1 : 0);
    }

    public static int not(int lit) {
        return lit ^ 1;
    }

    public static boolean sign(int lit) {
        return (lit & 1) == 1;
    }

    public static int var(int lit) {
        return lit >> 1;
    }

    protected MSJVariable v(int lit) {
        return vars.get(lit >> 1);
    }

    protected LBool value(int lit) {
        return sign(lit) ? LBool.negate(v(lit).assignment()) : v(lit).assignment();
    }

    //////////////////////////////////
    // Variable & clause management //
    //////////////////////////////////
    public int newVar() {
        int index = vars.size();
        MSJVariable newVar = new MSJVariable(index);
        vars.push(newVar);
        varHeap.insert(newVar);
        watches.push(new Vec<MSJClause>());
        watches.push(new Vec<MSJClause>());
        seen.push(false);
        return index;
    }

    public void newClause(IntVec clauseVec, boolean learnt) {
        if (!ok) {
            return;
        }
        IntVec simplifiedLearnt = null;
        if (!learnt) {
            simplifiedLearnt = simplifyProblemClause(clauseVec);
            if (simplifiedLearnt == null) {
                return;
            }
        }
        IntVec clause = learnt ? clauseVec : simplifiedLearnt;
        if (clause.size() == 0) {
            ok = false;
        } else if (clause.size() == 1) {
            if (!enqueue(clause.get(0), null)) {
                ok = false;
            }
        } else if (clause.size() == 2) {
            addBinaryClause(clause, learnt);
        } else {
            addNAryClause(clause, learnt);
        }
    }

    private IntVec simplifyProblemClause(IntVec clauseVec) {
        IntVec clause = new IntVec(clauseVec.size());
        clauseVec.copyTo(clause);
        clause.sortUnique();
        for (int i = 0; i < clause.size() - 1; i++)
            if (clause.get(i) == not(clause.get(i + 1))) {
                return null;
            }
        for (int i = 0; i < clause.size(); i++)
            if (value(clause.get(i)) == LBool.TRUE) {
                return null;
            }
        int i, j;
        for (i = j = 0; i < clause.size(); i++)
            if (value(clause.get(i)) != LBool.FALSE) {
                clause.set(j++, clause.get(i));
            }
        clause.shrink(i - j);
        return clause;
    }

    private void addBinaryClause(IntVec clauseVec, boolean learnt) {
        watches.get(not(clauseVec.get(0))).push(new MSJClause(1, clauseVec.get(1)));
        watches.get(not(clauseVec.get(1))).push(new MSJClause(1, clauseVec.get(0)));
        if (learnt) {
            enqueue(clauseVec.get(0), new MSJClause(1, not(clauseVec.get(1))));
            stats.learnts_literals += clauseVec.size();
        } else {
            stats.clauses_literals += clauseVec.size();
        }
        stats.n_bin_clauses++;
    }

    private void addNAryClause(IntVec clauseVec, boolean learnt) {
        MSJClause clause = new MSJClause(learnt, clauseVec);
        if (learnt) {
            int sndMax = 1;
            int max = v(clauseVec.get(1)).level();
            for (int i = 2; i < clauseVec.size(); i++)
                if (v(clauseVec.get(i)).level() > max) {
                    max = v(clauseVec.get(i)).level();
                    sndMax = i;
                }
            clause.set(1, clauseVec.get(sndMax));
            clause.set(sndMax, clauseVec.get(1));
            claBumpActivity(clause);
            enqueue(clause.get(0), clause);
            learnts.push(clause);
            stats.learnts_literals += clause.size();
        } else {
            clauses.push(clause);
            stats.clauses_literals += clause.size();
        }
        watches.get(not(clause.get(0))).push(clause);
        watches.get(not(clause.get(1))).push(clause);
    }

    private void remove(MSJClause clause) {
        if (clause.size() == 2) {
            removeWatch(watches.get(not(clause.get(0))), new MSJClause(1, clause.get(1)));
            removeWatch(watches.get(not(clause.get(1))), new MSJClause(1, clause.get(0)));
        } else {
            removeWatch(watches.get(not(clause.get(0))), clause);
            removeWatch(watches.get(not(clause.get(1))), clause);
        }
        if (clause.learnt()) {
            stats.learnts_literals -= clause.size();
        } else {
            stats.clauses_literals -= clause.size();
        }
    }

    private boolean removeWatch(IVec<MSJClause> watches, MSJClause clause) {
        if (watches.size() == 0) {
            return false;
        }
        int j;
        for (j = 0; !watches.get(j).equals(clause); j++)
            assert (j < watches.size()) : "There was a problem removing a watcher";
        for (; j < watches.size() - 1; j++)
            watches.set(j, watches.get(j + 1));
        watches.pop();
        return true;
    }

    private boolean locked(MSJClause clause) {
        MSJClause r = v(clause.get(0)).reason();
        if (r == null) {
            return false;
        }
        return !r.isLit() && r == clause;
    }

    /////////////////////////
    // Main CDCL functions //
    /////////////////////////
    private LBool search(int nof_conflicts) {
        if (!ok) {
            return LBool.FALSE;
        }
        stats.starts++;
        int conflCount = 0;
        model.clear();
        while (true) {
            MSJClause confl = propagate();
            if (confl != null) {
                stats.conflicts++;
                conflCount++;
                IntVec learntClause = new IntVec();
                if (decisionLevel() == rootLevel) {
                    analyzeFinal(confl, false);
                    return LBool.FALSE;
                }
                int backtrackLevel = analyze(confl, learntClause);
                cancelUntil(backtrackLevel > rootLevel ? backtrackLevel : rootLevel);
                newClause(learntClause, true);
                if (learntClause.size() == 1) {
                    v(learntClause.get(0)).setLevel(0);
                }
                claDecayActivity();
                if (--learntsize_adjust_cnt == 0) {
                    learntsize_adjust_confl *= learntsize_adjust_inc;
                    learntsize_adjust_cnt = (int) learntsize_adjust_confl;
                    max_learnts *= learntsize_inc;
                    if (params.log) {
                        System.out.printf("| %9d | %7d %8d | %7d %7d %8d %7.1f |\n",
                                (int) stats.conflicts,
                                nClauses(),
                                (int) stats.clauses_literals,
                                (int) max_learnts, learnts.size(),
                                (int) stats.learnts_literals,
                                (double) stats.learnts_literals / learnts.size());
                    }
                }
            } else {
                if (nof_conflicts >= 0 && conflCount >= nof_conflicts) {
                    cancelUntil(rootLevel);
                    return LBool.UNDEF;
                }
                if (decisionLevel() == 0) {
                    simplifyDB();
                }
                if (learnts.size() - trail.size() >= max_learnts) {
                    reduceDB();
                }
                stats.decisions++;
                if (stats.decisions % params.var_decay_rate == 0) {
                    decayVarActivity();
                }
                int next = pickBranchLit();
                if (next == -1) {
                    for (int i = 0; i < vars.size(); i++)
                        model.push(value(mkLit(i, false)) == LBool.TRUE);
                    cancelUntil(rootLevel);
                    return LBool.TRUE;
                }
                assume(next);
            }
        }
    }

    protected MSJClause propagate() {
        MSJClause confl = null;
        while (qhead < trail.size()) {
            stats.propagations++;
            stats.simpDBProps--;
            int propLit = trail.get(qhead++);
            IVec<MSJClause> watchers = watches.get(propLit);
            int i = 0;
            int j = 0;
            while (i != watchers.size()) {
                if (watchers.get(i).isLit()) {
                    MSJClause unitClause = watchers.get(i);
                    if (!enqueue(unitClause.lit(), new MSJClause(1, propLit))) {
                        if (decisionLevel() == 0) {
                            ok = false;
                        }
                        confl = new MSJClause(2);
                        confl.set(1, not(propLit));
                        confl.set(0, unitClause.lit());
                        qhead = trail.size();
                        while (i < watchers.size())
                            watchers.set(j++, watchers.get(i++));
                    } else {
                        watchers.set(j++, watchers.get(i++));
                    }
                } else {
                    MSJClause c = watchers.get(i);
                    i++;
                    // Make sure the false literal is data[1]:
                    int false_lit = not(propLit);
                    if (c.get(0) == false_lit) {
                        c.set(0, c.get(1));
                        c.set(1, false_lit);
                    }
                    // If 0th watch is true, then clause is already satisfied.
                    int first = c.get(0);
                    if (value(first) == LBool.TRUE) {
                        watchers.set(j++, c);
                    } else {
                        // Look for new watch:
                        boolean foundWatch = false;
                        for (int k = 2; k < c.size() && !foundWatch; k++)
                            if (value(c.get(k)) != LBool.FALSE) {
                                c.set(1, c.get(k));
                                c.set(k, false_lit);
                                watches.get(not(c.get(1))).push(c);
                                foundWatch = true;
                            }
                        // Did not find watch -- clause is unit under assignment
                        if (!foundWatch) {
                            watchers.set(j++, c);
                            if (!enqueue(first, c)) {
                                if (decisionLevel() == 0) {
                                    ok = false;
                                }
                                confl = c;
                                qhead = trail.size();
                                while (i < watchers.size())
                                    watchers.set(j++, watchers.get(i++));
                            }
                        }
                    }
                }
            }
            watchers.shrink(i - j);
        }
        return confl;
    }

    protected int analyze(MSJClause conflictClause, IntVec learntVec) {
        MSJClause confl = conflictClause;
        int pathCounter = 0;
        int conflictLit = litUndef;
        learntVec.push(-1);
        int backtrackLevel = 0;
        int index = trail.size() - 1;
        MSJClause temp = new MSJClause(2);
        do {
            if (confl.isLit()) {
                temp.set(1, confl.lit());
            }
            MSJClause c = confl.isLit() ? temp : confl;
            if (c.learnt()) {
                claBumpActivity(c);
            }
            for (int j = (conflictLit == litUndef) ? 0 : 1; j < c.size(); j++) {
                int q = c.get(j);
                if (!seen.get(var(q)) && v(q).level() > 0) {
                    v(q).bumpActivity();
                    seen.set(var(q), true);
                    if (v(q).level() == decisionLevel()) {
                        pathCounter++;
                    } else {
                        learntVec.push(q);
                        backtrackLevel = backtrackLevel > v(q).level() ? backtrackLevel : v(q).level();
                    }
                }
            }
            while (!seen.get(var(trail.get(index--)))) ;
            conflictLit = trail.get(index + 1);
            confl = v(conflictLit).reason();
            seen.set(var(conflictLit), false);
            pathCounter--;
        } while (pathCounter > 0);
        learntVec.set(0, not(conflictLit));
        simplifyLearntClause(learntVec);
        return backtrackLevel;
    }

    private void simplifyLearntClause(IntVec learntVec) {
        int i, j;
        IntVec toClear = new IntVec();
        if (params.expensive_ccmin) {
            int minLevel = 0;
            for (i = 1; i < learntVec.size(); i++)
                minLevel |= 1 << (v(learntVec.get(i)).level() & 31);
            learntVec.copyTo(toClear);
            for (i = j = 1; i < learntVec.size(); i++)
                if (v(learntVec.get(i)).reason() == null || !analyzeRemovable(learntVec.get(i), minLevel, toClear)) {
                    learntVec.set(j++, learntVec.get(i));
                }
        } else {
            learntVec.copyTo(toClear);
            for (i = j = 1; i < learntVec.size(); i++) {
                MSJClause r = v(learntVec.get(i)).reason();
                if (r == null) {
                    learntVec.set(j++, learntVec.get(i));
                } else if (r.isLit()) {
                    int q = r.lit();
                    if (!seen.get(var(q)) && v(q).level() != 0) {
                        learntVec.set(j++, learntVec.get(i));
                    }
                } else {
                    MSJClause c = r;
                    for (int k = 1; k < c.size(); k++)
                        if (!seen.get(var(c.get(k))) && v(c.get(k)).level() != 0) {
                            learntVec.set(j++, learntVec.get(i));
                            break;
                        }
                }
            }
        }
        stats.max_literals += learntVec.size();
        learntVec.shrink(i - j);
        stats.tot_literals += learntVec.size();
        for (int l = 0; l < toClear.size(); l++)
            seen.set(var(toClear.get(l)), false);
    }

    private boolean analyzeRemovable(int lit, int minLevel, IntVec toClear) {
        IntVec stack = new IntVec();
        stack.push(lit);
        int top = toClear.size();
        MSJClause temp = new MSJClause(2);
        while (stack.size() > 0) {
            assert (v(stack.last()).reason() != null);
            MSJClause r = v(stack.last()).reason();
            stack.pop();
            if (r.isLit()) {
                temp.set(1, r.lit());
            }
            MSJClause c = r.isLit() ? temp : r;
            for (int i = 1; i < c.size(); i++) {
                int p1 = c.get(i);
                if (!seen.get(var(p1)) && v(p1).level() != 0) {
                    if (v(p1).reason() != null && ((1 << (v(p1).level() & 31)) & minLevel) != 0) {
                        seen.set(var(p1), true);
                        stack.push(p1);
                        toClear.push(p1);
                    } else {
                        for (int j = top; j < toClear.size(); j++)
                            seen.set(var(toClear.get(j)), false);
                        toClear.shrink(toClear.size() - top);
                        return false;
                    }
                }
            }
        }
        return true;
    }

    protected void analyzeFinal(MSJClause confl, boolean skipFirst) {
        conflict.clear();
        if (rootLevel == 0) {
            return;
        }
        for (int i = skipFirst ? 1 : 0; i < confl.size(); i++) {
            int x = var(confl.get(i));
            if (v(x).level() > 0) {
                seen.set(x, true);
            }
        }
        int start = (rootLevel >= trailLimits.size()) ? trail.size() - 1 : trailLimits.get(rootLevel);
        for (int i = start; i >= trailLimits.get(0); i--) {
            int lit = trail.get(i);
            int var = var(lit);
            if (seen.get(var)) {
                MSJClause r = v(var).reason();
                if (r == null) {
                    assert (v(var).level() > 0);
                    conflict.push(not(trail.get(i)));
                } else {
                    if (r.isLit()) {
                        int p = r.lit();
                        if (v(var).level() > 0) {
                            seen.set(var(p), true);
                        }
                    } else {
                        MSJClause c = r;
                        for (int j = 1; j < c.size(); j++)
                            if (v(c.get(j)).level() > 0) {
                                seen.set(var(c.get(j)), true);
                            }
                    }
                }
                seen.set(var, false);
            }
        }
    }

    protected void cancelUntil(int level) {
        if (decisionLevel() > level) {
            for (int c = trail.size() - 1; c >= trailLimits.get(level); c--) {
                MSJVariable var = v(trail.get(c));
                var.assign(LBool.UNDEF);
                var.setReason(null);
                var.setPolarity(sign(trail.get(c)));
                if (varHeap.find(var) == -1) {
                    varHeap.insert(var);
                }
            }
            trail.shrink(trail.size() - trailLimits.get(level));
            trailLimits.shrink(trailLimits.size() - level);
            qhead = trail.size();
        }
    }

    private int pickBranchLit() {
        while (!varHeap.isEmpty()) {
            MSJVariable next = varHeap.heapExtractMax();
            if (next.assignment() == LBool.UNDEF) {
                return mkLit(next.num(), next.polarity());
            }
        }
        return -1;
    }

    protected boolean assume(int lit) {
        trailLimits.push(trail.size());
        return enqueue(lit, null);
    }

    protected boolean enqueue(int lit, MSJClause reason) {
        if (value(lit) != LBool.UNDEF) {
            return value(lit) != LBool.FALSE;
        } else {
            MSJVariable var = v(lit);
            var.assign(LBool.fromBool(!sign(lit)));
            var.setLevel(decisionLevel());
            var.setReason(reason);
            trail.push(lit);
            return true;
        }
    }

    //////////////////////////////////////////
    // Clause DB Simplification & Reduction //
    //////////////////////////////////////////
    private void reduceDB() {
        int i, j;
        double limit = params.cla_inc / learnts.size();
        learnts.sort(MSJClause.comp);
        for (i = j = 0; i < learnts.size() / 2; i++)
            if (learnts.get(i).size() > 2 && !locked(learnts.get(i))) {
                remove(learnts.get(i));
            } else {
                learnts.set(j++, learnts.get(i));
            }
        for (; i < learnts.size(); i++)
            if (learnts.get(i).size() > 2 && !locked(learnts.get(i)) && learnts.get(i).activity() < limit) {
                remove(learnts.get(i));
            } else {
                learnts.set(j++, learnts.get(i));
            }
        learnts.shrink(i - j);
    }

    private void simplifyDB() {
        if (!ok) {
            return;
        }
        if (propagate() != null) {
            ok = false;
            return;
        }
        if (trail.size() == stats.simpDBAssigns || stats.simpDBProps > 0) {
            return;
        }
        for (int i = stats.simpDBAssigns; i < trail.size(); i++) {
            int p = trail.get(i);
            IVec<MSJClause> watchers = watches.get(not(p));
            for (int j = 0; j < watchers.size(); j++)
                if (watchers.get(j).isLit()) {
                    if (removeWatch(this.watches.get(not(watchers.get(j).lit())), new MSJClause(1, p))) {
                        stats.n_bin_clauses--;
                    }
                }
            this.watches.get(p).clear();
            this.watches.get(not(p)).clear();
        }
        // Remove satisfied clauses:
        for (int type = 0; type < 2; type++) {
            IVec<MSJClause> cs = type == 1 ? learnts : clauses;
            int j = 0;
            for (int i = 0; i < cs.size(); i++) {
                if (!locked(cs.get(i)) && canBeSimplified(cs.get(i))) {
                    remove(cs.get(i));
                } else {
                    cs.set(j++, cs.get(i));
                }
            }
            cs.shrink(cs.size() - j);
        }
        stats.simpDBAssigns = trail.size();
        stats.simpDBProps = stats.clauses_literals + stats.learnts_literals;
    }

    private boolean canBeSimplified(MSJClause c) {
        for (int i = 0; i < c.size(); i++) {
            if (value(c.get(i)) == LBool.TRUE) {
                return true;
            }
        }
        return false;
    }

    ////////////////
    // Activities //
    ////////////////
    private void decayVarActivity() {
        for (int i = 1; i < vars.size(); i++) {
            MSJVariable var = vars.get(i);
            var.decayActivity(params.var_decay);
        }
        varHeap.restoreHeapProperty();
    }

    private void claBumpActivity(MSJClause clause) {
        clause.bumpActivity(params.cla_inc);
        if (clause.activity() > 1e20) {
            claRescaleActivity();
        }
    }

    private void claDecayActivity() {
        params.cla_inc *= params.cla_decay;
    }

    private void claRescaleActivity() {
        for (int i = 0; i < learnts.size(); i++)
            learnts.get(i).rescaleActivity();
        params.cla_inc *= 1e-20;
    }

    //////////////////////
    // Internal helpers //
    //////////////////////
    protected int decisionLevel() {
        return trailLimits.size();
    }

    private int nClauses() {
        return clauses.size() + stats.n_bin_clauses;
    }

    private static double luby(double y, int x) {
        int size, seq;
        for (size = 1, seq = 0; size < x + 1; seq++, size = 2 * size + 1) ;
        while (size - 1 != x) {
            size = (size - 1) >> 1;
            seq--;
            x = x % size;
        }
        return Math.pow(y, seq);
    }

    //////////////////////////////////
    // Main entry point for solving //
    //////////////////////////////////
    public boolean solve(IntVec assumps) {
        simplifyDB();
        if (!ok) {
            return false;
        }
        max_learnts = nClauses() * learntsize_factor;
        learntsize_adjust_confl = learntsize_adjust_start_confl;
        learntsize_adjust_cnt = (int) learntsize_adjust_confl;
        LBool status = LBool.UNDEF;
        rootLevel = assumps.size();
        for (int i = 0; i < assumps.size(); i++) {
            int p = assumps.get(i);
            assert (var(p) < vars.size());
            if (!assume(p)) {
                MSJClause r = v(p).reason();
                if (r != null) {
                    MSJClause confl = null;
                    if (r.isLit()) {
                        confl = new MSJClause(2);
                        confl.set(1, not(p));
                        confl.set(0, r.lit());
                    } else {
                        confl = r;
                    }
                    analyzeFinal(confl, true);
                    conflict.push(not(p));
                } else {
                    conflict.clear();
                    conflict.push(not(p));
                }
                cancelUntil(0);
                return false;
            }
            MSJClause confl = propagate();
            if (confl != null) {
                analyzeFinal(confl, false);
                assert (conflict.size() > 0);
                cancelUntil(0);
                return false;
            }
        }
        if (params.log) {
            System.out.print("===============================[MiniSAT Java]======================\n");
            System.out.print("| Conflicts |     ORIGINAL     |              LEARNT              |\n");
            System.out.print("|           | Clauses Literals |   Limit Clauses Literals  Lit/Cl |\n");
            System.out.print("===================================================================\n");
        }
        int curr_restarts = 0;
        while (status == LBool.UNDEF) {
            double rest_base = luby(restart_inc, curr_restarts);
            status = search((int) (rest_base * restart_first));
            curr_restarts++;
        }
        if (params.log) {
            System.out.print("===================================================================\n");
        }
        cancelUntil(0);
        return status == LBool.TRUE;
    }

    public boolean solve() {
        IntVec tmp = new IntVec();
        return solve(tmp);
    }

    //////////////////////////////////
    // Additional Stats             //
    //////////////////////////////////

    /**
     * Returns a list of literals which indicates the assignment of each variable.
     * <p/>
     * Unassigned variables are ignored and therefore not in this list.
     *
     * @return the list of literals which indicate the assignment
     */
    public List<Integer> getModel() {
        List<Integer> set = new LinkedList<Integer>();
        for (int i = 0; i < vars.size(); i++)
            set.add(MSJCoreProver.mkLit(i, !model.get(i)));
        return set;
    }
}
