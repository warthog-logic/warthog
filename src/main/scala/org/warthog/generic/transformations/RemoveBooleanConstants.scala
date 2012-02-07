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

package org.warthog.generic.transformations

import org.warthog.generic.formulas._

/**
 * Remove all Boolean constants of a formula
 *
 * If the formula is equivalent to `true` or `false` there will obviously be one constant in the result.
 *
 * Author: zengler
 * Date:   25.01.12
 */
trait RemoveBooleanConstants[L <: Logic] extends Transformation[L] {

  def removeBooleanConstants: Formula[L] = simplify(f)

  private def simplify(arg: Formula[L] = f): Formula[L] = arg match {
    case Not(p)            => psimplify1(Not(simplify(p)))
    case Implication(p, q) => psimplify1(Implication(simplify(p), simplify(q)))
    case Equiv(p, q)       => psimplify1(Equiv(simplify(p), simplify(q)))
    case Xor(p, q)         => psimplify1(Xor(simplify(p), simplify(q)))
    case And(fs@_*)        => psimplify1(And(fs.map(simplify(_)): _*))
    case Or(fs@_*)         => psimplify1(Or(fs.map(simplify(_)): _*))
    case _                 => arg
  }

  private def psimplify1(f: Formula[L]): Formula[L] = f match {
    case Not(a: Falsum[L])            => Verum()
    case Not(a: Verum[L])             => Falsum()
    case Implication(a: Falsum[L], _) => Verum()
    case Implication(_, a: Verum[L])  => Verum()
    case Implication(a: Verum[L], p)  => p
    case Implication(p, a: Falsum[L]) => Not(p)
    case Equiv(p, a: Verum[L])        => p
    case Equiv(a: Verum[L], p)        => p
    case Equiv(a: Falsum[L], p)       => Not(p)
    case Equiv(p, a: Falsum[L])       => Not(p)
    case Xor(p, a: Verum[L])          => Not(p)
    case Xor(a: Verum[L], p)          => Not(p)
    case Xor(a: Falsum[L], p)         => p
    case Xor(p, a: Falsum[L])         => p
    case And(fs@_*)                   => simplifyAnd(fs: _*)
    case Or(fs@_*)                    => simplifyOr(fs: _*)
    case _                            => f
  }

  private def simplifyAnd(fs: Formula[L]*): Formula[L] = {
    val newAnd = for {
      f <- fs
      if (f match {
        case a: Falsum[L] => return Falsum()
        case a: Verum[L]  => false
        case _            => true
      })
    } yield f
    newAnd.size match {
      case 0 => Verum()
      case 1 => newAnd.head
      case _ => And(newAnd: _*)
    }
  }

  private def simplifyOr(fs: Formula[L]*): Formula[L] = {
    val newOr = for {
      f <- fs
      if (f match {
        case a: Verum[L]  => return Verum()
        case a: Falsum[L] => false
        case _            => true
      })
    } yield f
    newOr.size match {
      case 0 => Falsum()
      case 1 => newOr.head
      case _ => Or(newOr: _*)
    }
  }
}
