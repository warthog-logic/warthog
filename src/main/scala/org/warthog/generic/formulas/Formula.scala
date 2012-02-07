/**
 * Copyright (c) 2011, Andreas J. Kuebler & Christoph Zengler
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.warthog.generic.formulas

import org.warthog.pl.formulas.PL

/**
 * Trait for a Formula
 *
 * Author: zengler
 * Date:   25.01.12
 */
abstract class Formula[-L <: Logic] extends Term[PL] {
  type FAtom = Atom[L#AtomLogic]
  type FVariable = Variable[L#VariableLogic]
  type FTerm = Term[L#TermLogic]

  /**
   * Aliases
   */
  def unary_- : Formula[L] = Not(this)

  def &&[T <: L](that: Formula[T]) = And(this, that)

  def ||[T <: L](that: Formula[T]) = Or(this, that)

  /**
   * The set of atoms
   * @return the set of atoms of this formula
   */
  def atoms: List[FAtom]

  /**
   * The set of variables
   * @return the set of variables of this formula
   */
  def vars: List[FVariable]

  /**
   * The set of free variables
   * @return the set of free variables of this formula
   */
  def freeVars: List[FVariable]

  /**
   * The set of bound variables
   * @return the set of bound variables of this formula
   */
  def boundVars: List[FVariable]

  /**
   * The number of variables of this formula
   * @return the number of variables of this formula
   */
  lazy val numOfVars: Int = vars.size

  /**
   * The number of atoms of this formula
   * @return the number of atoms of this formula
   */
  def numOfAtoms: Int

  /**
   * The number of nodes of this formula
   * @return the number of nodes of this formula
   */
  def numOfNodes: Int

  /**
   * Print the statistics for this formula
   */
  def stats: String = {
    "Number of variables: %d\n".format(numOfVars) +
      "Number of atoms: %d\n".format(numOfAtoms) +
      "Number of nodes: %d\n".format(numOfNodes)
  }

  def syntacticalRewrite[T <: L](sub: Formula[T], subwith: Formula[T]): Formula[T] = syntacticalRewrite(Map(sub -> subwith))

  def syntacticalRewrite[T <: L](subs: Map[Formula[T], Formula[T]]): Formula[T]

  /**
   * Return a representation of the formula only containing ''not'', ''and'' and ''or'' connectives
   * @return the flattened formula
   */
  def booleanFlatten: Formula[L] = this

  /**
   * Negation Normal Form (NNF)
   * @return the NNF of the formula
   */
  def nnf: Formula[L] = booleanFlatten.getNNF(true)

  def getNNF(phase: Boolean): Formula[L]

  /**
   * Is the formula ground (no variables)
   * @return `true` if the formula is ground, `false` otherwise
   */
  def isGround: Boolean = freeVars.isEmpty

  /**
   * Is the formula ground (no variables)
   * @return `true` if the formula is ground, `false` otherwise
   */
  def isLiteral: Boolean = false

  /**
   * Is the formula in Negation Normal Form (NNF)
   * @return `true` if the formula is in NNF, `false` otherwise
   */
  def isNNF: Boolean

  /**
   * Return the priority of this formula (for pretty printing)
   * @return the prioirty of the formula
   */
  def priority: Int
}

/**
 * Companion object for general formula constants and methods
 */
object Formula {
  // UTF8 representations of Boolean operators
  val TRUE = "\u22a4"
  val FALSE = "\u22a5"
  val NOT = "\u00ac"
  val XOR = "\u2295"
  val IMPL = "\u2192"
  val EQUIV = "\u2194"
  val AND = "\u2227"
  val OR = "\u2228"
  val FORALL = "\u2200"
  val EXISTS = "\u2203"

  /**
   * Apply org Morgans Law `-(a1 /\ a2) <=> (-a1 \/ -a2)`
   * @param form a conjunction or a disjunction
   * @return the formula with org Morgan's Law applied
   */
  def deMorgan[L <: Logic](form: Formula[L]): Formula[L] = form match {
    case And(fs@_*) => Or[L](fs.map(Not(_).nnf): _*)
    case Or(fs@_*)  => And[L](fs.map(Not(_).nnf): _*)
  }
}
