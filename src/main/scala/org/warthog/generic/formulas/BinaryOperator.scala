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

/**
 * Abstract case class for a binary operator (e.g. implication, equivalence, xor)
 * @param op the string representing the operator
 * @param f1 a formula
 * @param f2 a formula
 *
 * Author: zengler
 * Date:   25.01.12
 */
abstract class BinaryOperator[-L <: Logic](val op: String, val f1: Formula[L], val f2: Formula[L]) extends Formula[L] {

  override def equals(a: Any) = a match {
    case n: BinaryOperator[L] => n.op == op && n.f1 == f1 && n.f2 == f2
    case _                    => false
  }

  def atoms = (f1.atoms union f2.atoms).distinct

  def vars = (f1.vars union f2.vars).distinct

  def freeVars = (f1.freeVars union f2.freeVars).distinct

  def boundVars = (f1.boundVars union f2.boundVars).distinct

  def numOfAtoms = f1.numOfAtoms + f2.numOfAtoms

  def numOfNodes = 1 + f1.numOfNodes + f2.numOfNodes

  def getNNF(phase: Boolean) = booleanFlatten.getNNF(phase)

  override def isNNF = false
}

object BinaryOperator {
  def unapplySeq[L <: Logic](f: Formula[L]) = f match {
    case n: BinaryOperator[L] => Some((n.op, n.f1, n.f2))
    case _                    => None
  }
}
