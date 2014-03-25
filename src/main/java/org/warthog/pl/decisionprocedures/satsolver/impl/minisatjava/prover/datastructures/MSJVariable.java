package org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.datastructures;

import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections.ComparableWithIndex;

/**
 * Variables
 */
public class MSJVariable implements ComparableWithIndex<MSJVariable> {
  private int num;
  private LBool assignment = LBool.UNDEF;
  private int level = -1;
  private MSJClause reason = null;
  private int activity = 0;
  private boolean polarity = false;
  private int index;

  public MSJVariable(int num) {
    this.num = num;
  }

  @Override
  public int index() {
    return index;
  }

  @Override
  public void setIndex(int i) {
    index = i;
  }

  @Override
  public int compareTo(MSJVariable variable) {
    return activity - variable.activity;
  }

  public int num() {
    return num;
  }

  public void setLevel(int level) {
    this.level = level;
  }

  public int level() {
    return level;
  }

  public void setReason(MSJClause reason) {
    this.reason = reason;
  }

  public MSJClause reason() {
    return reason;
  }

  public void assign(LBool assignment) {
    this.assignment = assignment;
  }

  public LBool assignment() {
    return assignment;
  }

  public void bumpActivity() {
    activity++;
  }

  public void decayActivity(int varRescale) {
    activity /= varRescale;
  }

  public void setPolarity(boolean polarity) {
    this.polarity = polarity;
  }

  public boolean polarity() {
    return polarity;
  }

  @Override
  public String toString() {
    return "" + num;
  }

  @Override
  public int hashCode() {
    return num();
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) { return true; }
    if (o instanceof MSJVariable) { return ((MSJVariable) o).num == num; }
    return false;
  }
}
