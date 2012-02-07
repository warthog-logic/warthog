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
 * Abstract case class for a quantifier (e.g. exists, forall)
 * @param quant the string representing the quantifier
 * @param x the set of quantified variables
 * @param arg a formula
 *
 * Author: zengler
 * Date:   25.01.12
 */
abstract class Quantifier[-L <: QuantifiedLogic](val quant: String, val x: Variable[L#VariableLogic], val arg: Formula[L]) extends Formula[L] {

  def atoms = arg.atoms

  def vars = (x :: arg.vars).distinct

  override def equals(f: Any) = f match {
    case q: Quantifier[L] => q.quant == quant && q.x == x && q.arg == arg
    case _                => false
  }

  def freeVars = arg.freeVars.filterNot(e => e == x)

  def boundVars = (x :: arg.boundVars).distinct

  def numOfAtoms = arg.numOfAtoms

  def numOfNodes = 1 + arg.numOfNodes

  def isNNF = arg.isNNF

  def priority = 50
}

object Quantifier {
  def unapplySeq[L <: Logic](f: Formula[L]) = f match {
    case q: Quantifier[L] => Some((q.quant, q.x, q.arg))
    case _                => None
  }
}
