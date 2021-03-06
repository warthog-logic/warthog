/*
 * Copyright (c) 2011-2014, Andreas J. Kuebler & Christoph Zengler
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
  * Case Class for an n-ary logical 'or'
  *
  * Formulas are automatically compactified, e.g. `Or(Or(x,y),Or(a,b))` is transformed to `Or(x,y,a,b)`
  * @param fs the list of generic
  * @tparam L The logic of the formula
  *
  * ATTENTION: you should not use `fs` to access the argument list, but `args`.  In args the compacified
  * arguments are stored.
  */
class Or[-L <: Logic](fs: Formula[L]*)
    extends NAryOperator[L](Formula.OR, NAryOperator.compactify(Formula.OR, fs: _*): _*) {

  override def equals(t: Any) = t match {
    case a: Or[L] => super.equals(t)
    case _        => false
  }

  override def hashCode() = args.foldLeft(0)(_ | _.##)

  override def booleanFlatten = Or(args.map(_.booleanFlatten): _*)

  def syntacticalRewrite[T <: L](subs: Map[Formula[T], Formula[T]]) = subs.get(this) match {
    case Some(p) => p
    case None    => Or(args.map(_.syntacticalRewrite(subs)): _*)
  }

  def getNNF(phase: Boolean) = if (phase) Or(args.map(_.getNNF(true)): _*) else Formula.deMorgan(Not(this))

  def priority = 30
}

object Or {
  def apply[L <: Logic](fs: Formula[L]*) = {
    new Or[L](fs: _*)
  }

  def unapplySeq[L <: Logic](f: Formula[L]) = {
    f match {
      case a: Or[L] => Some(a.args)
      case _        => None
    }
  }
}
