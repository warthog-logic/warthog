package org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.datastructures;

import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections.nativeType.IntVec;

import java.util.Comparator;

/**
 * Clauses
 */
public class MSJClause {
  private IntVec data;
  private boolean learnt;
  private double activity = 0;
  public final static Comparator<MSJClause> comp = new Comparator<MSJClause>() {
    @Override
    public int compare(MSJClause c1, MSJClause c2) {
      if (c1.activity == c2.activity) { return 0; }
      return c1.activity() < c2.activity() ? -1 : 1;
    }
  };

  public MSJClause(boolean learnt, IntVec ps) {
    this.learnt = learnt;
    data = new IntVec(ps.size());
    for (int i = 0; i < ps.size(); i++)
      data.unsafePush(ps.get(i));
  }

  public MSJClause(int size) {
    learnt = false;
    data = new IntVec(size, -1);
  }

  public MSJClause(int size, int lit) {
    learnt = false;
    data = new IntVec(size);
    data.push(lit);
  }

  public int size() {
    return data.size();
  }

  public IntVec getLits() {
    return data;
  }

  public boolean learnt() {
    return learnt;
  }

  public int get(int i) {
    return data.get(i);
  }

  public void set(int i, int l) {
    data.set(i, l);
  }

  public boolean isLit() {
    return size() == 1;
  }

  public int lit() {
    return data.get(0);
  }

  public void bumpActivity(double bump) {
    activity += bump;
  }

  public void rescaleActivity() {
    activity *= 1e-20;
  }

  public double activity() {
    return activity;
  }

  @Override
  public String toString() {
    return "[" + data.toString() + "]";
  }

  @Override
  public int hashCode() {
    return data.hashCode();
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) { return true; }
    if (o instanceof MSJClause) {
      MSJClause other = (MSJClause) o;
      return other.data.equals(data) && other.learnt == learnt && other.activity == activity;
    }
    return false;
  }
}
