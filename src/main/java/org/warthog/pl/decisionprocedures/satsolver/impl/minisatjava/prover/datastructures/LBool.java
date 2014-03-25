package org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.datastructures;

/**
 * Lifted booleans
 */
public enum LBool {
  TRUE, FALSE, UNDEF;

  public static LBool negate(LBool l) {
    return l == FALSE ? TRUE : (l == TRUE ? FALSE : UNDEF);
  }

  public static LBool fromBool(boolean b) {
    return b ? TRUE : FALSE;
  }
}
