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

package org.warthog.fol.transformations

import org.warthog.generic.formulas._
import org.warthog.fol.formulas._
import org.warthog.generic.transformations.Transformation

/**
 * FOL Skolemization
 *
 * Author: zengler
 * Date:   31.01.12
 */

trait Skolemization extends Transformation[FOL] {

  def skolemize: Formula[FOL] = specialize(askolemize(f).pnf)

  private def skolem(arg: Formula[FOL], funs: List[FunctionSymbol]): (Formula[FOL], List[FunctionSymbol]) = arg match {
    case FOLExists(y, p) => {
      val freeVars = arg.freeVars.asInstanceOf[List[FOLVariable]]
      val newFunctionSymbol = getSkolemFunction(arg, freeVars.size)
      val newFunction = FOLFunction(newFunctionSymbol, freeVars.toSeq: _*)
      skolem(p.substitute(y, newFunction), newFunctionSymbol :: funs)
    }
    case FOLForAll(x, p) => {
      val (newP, newFuns) = skolem(p, funs)
      (FOLForAll(x, newP), newFuns)
    }
    case And(fs@_*)      => skolem2(Formula.AND, funs, fs: _*)
    case Or(fs@_*)       => skolem2(Formula.OR, funs, fs: _*)
    case _               => (arg, funs)
  }

  private def skolem2(op: String, funs: List[FunctionSymbol], fs: Formula[FOL]*) = {
    val init = (Seq[Formula[FOL]](), funs)
    val newSkolem = fs.foldLeft(init)((t, e) => {
      val temp = skolem(e, t._2);
      (t._1.:+(temp._1), temp._2)
    })
    if (op == Formula.AND)
      (And(newSkolem._1: _*), newSkolem._2)
    else
      (Or(newSkolem._1: _*), newSkolem._2)
  }

  private def askolemize(f: Formula[FOL]): Formula[FOL] = {
    val form = f.nnf
    skolem(form, form.functions)._1
  }

  private def specialize(f: Formula[FOL]): Formula[FOL] = f match {
    case FOLForAll(x, p) => specialize(p)
    case _               => f
  }

  private def getSkolemFunction(arg: Formula[FOL], arity: Int, i: Int = 0): FunctionSymbol = {
    val funs = arg.functions.filter(_.name == "sk" + i)
    if (funs.isEmpty) FunctionSymbol("sk" + i, arity) else getSkolemFunction(arg, arity, i + 1)
  }
}
