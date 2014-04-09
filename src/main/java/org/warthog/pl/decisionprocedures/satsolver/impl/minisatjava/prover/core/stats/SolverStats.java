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

package org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.core.stats;

/**
 * Solver statistics
 */
public class SolverStats {
  public long starts = 0;
  public long decisions = 0;
  public long propagations = 0;
  public long conflicts = 0;
  public long clauses_literals = 0;
  public long learnts_literals = 0;
  public long max_literals = 0;
  public long tot_literals = 0;
  public int n_bin_clauses = 0;
  public int simpDBAssigns = 0;
  public long simpDBProps = 0;
}
