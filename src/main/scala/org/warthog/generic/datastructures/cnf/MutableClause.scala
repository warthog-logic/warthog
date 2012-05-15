package org.warthog.generic.datastructures.cnf

import org.warthog.generic.formulas.Logic

/**
 * Base class for a mutable clause
 *
 * Author: zengler
 * Date:   14.05.12
 */
abstract class MutableClause[L <: Logic, T <: Literal[L]](ls: List[T]) extends ClauseLike[L, T] {

  protected val _lits = ls.distinct.toBuffer

  /**
   * The sequence of literals in this clause
   * @return the list of literals
   */
  def literals: List[T] = _lits.toList

  /**
   * Delete a literal in this clause
   * @param lit a literal
   */
  def delete(lit: T) = {
    _lits -= lit
    this
  }

  /**
   * Push a literal to this clause
   * @param lit a literal
   */
  def push(lit: T) = {
    if (!_lits.contains(lit))
      _lits += lit
    this
  }

  /**
   * Add a number of literals to this clause
   * @param lits a list of literals
   */
  def pushLiterals(lits: T*) = {
    for (l <- lits)
      push(l)
    this
  }
}
