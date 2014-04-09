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
    return "" + num + assignment();
  }

  @Override
  public int hashCode() {
    return num();
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o instanceof MSJVariable) {
      return ((MSJVariable) o).num == num;
    }
    return false;
  }
}
