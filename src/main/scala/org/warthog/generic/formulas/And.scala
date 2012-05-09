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
 * Case Class for an n-ary and
 *
 * Formulas are automatically compactified, e.g. `And(And(x,y),And(a,b))` is transformed to `And(x,y,a,b)`
 * @param fs the list of generic
 *
 * ATTENTION: you should not use `fs` to access the argument list, but `args`.  In args the compacified
 * arguments are stored.
 *
 * Author: zengler
 * Date:   25.01.12
 */
class And[-L <: Logic](fs: Formula[L]*) extends NAryOperator[L](Formula.AND, NAryOperator.compactify(Formula.AND, fs: _*): _*) {

  override def toString = "(" + args.mkString(" & ") + ")"

  override def equals(t: Any) = {
    t match {
      case a: And[L] => super.equals(t)
      case _         => false
    }
  }

  override def hashCode() = args.foldLeft(-1)(_ & _.##)

  override def booleanFlatten = And(args.map(_.booleanFlatten): _*)

  def syntacticalRewrite[T <: L](subs: Map[Formula[T], Formula[T]]) = subs.get(this) match {
    case Some(p) => p
    case None    => And(args.map(_.syntacticalRewrite(subs)): _*)
  }

  def getNNF(phase: Boolean) = if (phase) And(args.map(_.getNNF(true)): _*) else Formula.deMorgan(this)

  def priority = 40
}

/**
 * Companion object to simulate a case class
 *
 * This is necessary because in Pattern matching we want access to the compactified version of the parameters.
 */
object And {
  def apply[L <: Logic](fs: Formula[L]*) = {
    new And[L](fs: _*)
  }

  def unapplySeq[L <: Logic](f: Formula[L]) = {
    f match {
      case a: And[L] => Some(a.args)
      case _         => None
    }
  }
}
