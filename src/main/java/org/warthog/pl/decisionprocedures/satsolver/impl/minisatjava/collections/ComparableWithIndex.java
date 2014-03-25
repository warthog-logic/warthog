package org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections;

public interface ComparableWithIndex<T> extends Comparable<T> {
  int index();

  void setIndex(int i);
}
