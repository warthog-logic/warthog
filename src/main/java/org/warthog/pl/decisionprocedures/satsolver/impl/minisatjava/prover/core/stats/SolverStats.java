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
