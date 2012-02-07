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

package org.warthog.fol.formulas

import org.warthog.generic.formulas.{Formula, Quantifier}

/**
 * Case class for a FOL universal quantifier
 * @param v the quantified variable
 * @param form a formula
 *
 * Author: zengler
 * Date:   25.01.12
 */
case class FOLForAll(v: FOLVariable, form: Formula[FOL]) extends Quantifier[FOL](Formula.FORALL, v, form) {

  override def toString = "![%s]: %s".format(v, form)

  override def booleanFlatten = FOLForAll(v, form.booleanFlatten)

  def getNNF(phase: Boolean) =
    if (!form.freeVars.contains(v))
      form.getNNF(phase)
    else if (phase)
      FOLForAll(v, form.getNNF(true))
    else
      FOLExists(v, form.getNNF(false))

  def tsubst(s: Map[FOLVariable, FOLTerm]) = {
    val newSubst = s.filterKeys(_ != v)
    val nv = if (newSubst.values.exists(_.vars.contains(x))) v.freshVariable(form) else v
    FOLForAll(nv, form.substitute(newSubst + (v -> nv.asInstanceOf[FOLTerm])))
  }

  def syntacticalRewrite[T <: FOL](subs: Map[Formula[T], Formula[T]]) = subs.get(this) match {
    case Some(p) => p
    case None    => FOLForAll(v, form.syntacticalRewrite(subs).asInstanceOf[Formula[FOL]])
  }
}

object FOLForAll {
  def apply(a: Set[FOLVariable], form: Formula[FOL]): FOLForAll = a.size match {
    case 0 => throw new Exception("A quantifier must bind a variable.")
    case 1 => new FOLForAll(a.head, form)
    case _ => new FOLForAll(a.head, apply(a.tail, form))
  }
}
