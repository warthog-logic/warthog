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

import org.warthog.fol.formulas.{FOL, FOLExists, FOLForAll}
import org.warthog.generic.formulas._
import org.warthog.generic.transformations.Transformation

/**
 * FOL PNF Generation
 *
 * Author: zengler
 * Date:   25.01.12
 */
trait PNF extends Transformation[FOL] with Substitution {
  /**
   * Is a formula in PNF
   * @return true if the formula is in PNF, false otherwise
   */
  def isPNF: Boolean = pnfp(f)

  /**
   * Get the PNF of a formula
   * @return the PNF of the formula
   */
  def pnf: Formula[FOL] = pnf(f.nnf)

  private def pnfp(arg: Formula[FOL]): Boolean = arg match {
    case Not(p)                 => if (p.isInstanceOf[Quantifier[FOL]]) false else pnfp(p)
    case b: BinaryOperator[FOL] => (b.f1 match {
      case q: Quantifier[FOL] => false
      case _                  => pnfp(b.f1)
    }) && (b.f2 match {
      case q: Quantifier[FOL] => false
      case _                  => pnfp(b.f2)
    })
    case n: NAryOperator[FOL]   => n.args.forall(f => !f.isInstanceOf[Quantifier[FOL]] && pnfp(f))
    case q: Quantifier[FOL]     => pnfp(q.arg)
    case _                      => true
  }

  private def pnf(arg: Formula[FOL]): Formula[FOL] = arg match {
    case FOLForAll(x, p) => FOLForAll(x, pnf(p))
    case FOLExists(x, p) => FOLExists(x, pnf(p))
    case And(fs@_*)      => pullquants(Formula.AND, fs.map(pnf(_)): _*)
    case Or(fs@_*)       => pullquants(Formula.OR, fs.map(pnf(_)): _*)
    case _               => arg
  }

  private def pullquants(op: String, fs: Formula[FOL]*): Formula[FOL] = {
    if (fs.forall(!_.isInstanceOf[Quantifier[FOL]]))
      return if (op == Formula.AND) And(fs: _*) else Or(fs: _*)
    fs.find(_.isInstanceOf[FOLForAll]) match {
      case Some(q) => {
        val foundQuant = q.asInstanceOf[FOLForAll]
        val newVar = foundQuant.v.freshVariable(And(fs: _*))
        FOLForAll(newVar, pullquants(op, fs.updated(fs.indexOf(q), foundQuant.form.substitute(foundQuant.v, newVar)): _*))
      }
      case None    => fs.find(_.isInstanceOf[FOLExists]) match {
        case Some(q) => {
          val foundQuant = q.asInstanceOf[FOLExists]
          val newVar = foundQuant.v.freshVariable(And(fs: _*))
          FOLExists(newVar, pullquants(op, fs.updated(fs.indexOf(q), foundQuant.form.substitute(foundQuant.v, newVar)): _*))
        }
        case None    => throw new Exception("This should not happen at all")
      }
    }
  }
}
