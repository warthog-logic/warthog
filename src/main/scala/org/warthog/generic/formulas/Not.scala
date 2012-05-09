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
 * Case Class for a negation -arg
 * @param arg a formula
 *
 * Author: zengler
 * Date:   25.01.12
 */
class Not[-L <: Logic](protected val arg: Formula[L]) extends Formula[L] {

  override def toString = "~" + (if (arg.isLiteral) arg else "(" + arg + ")")

  override def equals(t: Any): Boolean = t match {
    case Not(form) => arg.equals(form)
    case _         => false
  }

  override def hashCode() = arg.## ^ 1

  def atoms = arg.atoms

  def vars = arg.vars

  def freeVars = arg.freeVars

  def boundVars = arg.boundVars

  def numOfAtoms = arg.numOfAtoms

  def numOfNodes = 1 + arg.numOfNodes

  override def booleanFlatten = Not(arg.booleanFlatten)

  def syntacticalRewrite[T <: L](subs: Map[Formula[T], Formula[T]]) = subs.get(this) match {
    case Some(p) => p
    case None    => Not(arg.syntacticalRewrite(subs))
  }

  def getNNF(phase: Boolean) = if (phase) arg.getNNF(false) else arg.getNNF(true)

  override def isLiteral = arg match {
    case a: Atom[L] => true
    case _          => false
  }

  def isNNF = arg match {
    case a: Atom[L] => true
    case _          => false
  }

  def priority = 60
}

/**
 * Companion object to simulate arg1 case class
 */
object Not {
  def apply[L <: Logic](f: Formula[L]) = f match {
    case Not(n) => n
    case _      => new Not[L](f)
  }

  def unapply[L <: Logic](f: Formula[L]): Option[Formula[L]] = f match {
    case a: Not[L] => Some(a.arg)
    case _         => None
  }
}
