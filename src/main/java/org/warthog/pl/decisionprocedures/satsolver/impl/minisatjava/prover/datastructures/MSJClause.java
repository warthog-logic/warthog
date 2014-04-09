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
      if (c1.activity == c2.activity) {
        return 0;
      }
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
    if (this == o) {
      return true;
    }
    if (o instanceof MSJClause) {
      MSJClause other = (MSJClause) o;
      return other.data.equals(data) && other.learnt == learnt && other.activity == activity;
    }
    return false;
  }
}
