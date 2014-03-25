package org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.core.stats;

/**
 * Search parameters
 */
public class SearchParams {
  public boolean expensive_ccmin = true;
  public double cla_inc = 1;
  public double cla_decay = 1;
  public int var_decay = 4;
  public int var_decay_rate = 256;
  public boolean log = true;
}
