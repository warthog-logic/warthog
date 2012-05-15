package org.warthog.generic.datastructures.cnf

import org.warthog.generic.formulas.{Logic, Formula}

/**
 * Trait for a literal
 *
 * Author: zengler
 * Date:   15.05.12
 */
trait Literal[L <: Logic] {

  /**
   * The phase of the literal
   * @return the phase
   */
  def phase: Boolean

  /**
   * A formula representation of the literal
   * @return a formula respresentation in propositional logic
   */
  def toFormula: Formula[L]

  /**
   * Return a negated copy of the literal
   * @return a negated copy of the literal
   */
  def negate: Literal[L]

}
