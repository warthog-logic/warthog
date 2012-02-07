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
 * Abstract case class for an n-ary operator (e.g. and, or)
 * @param op the string representing the operator
 * @param args a list of generic
 *
 * Author: zengler
 * Date:   25.01.12
 */
abstract class NAryOperator[-L <: Logic](val op: String, val args: Formula[L]*) extends Formula[L] {

  override def equals(a: Any) = a match {
    case n: NAryOperator[L] => n.op == op && args.length == n.args.length &&
      (0 until args.length).forall(i => args(i) == n.args(i))
    case _                  => false
  }

  def atoms = args.map(_.atoms).reduce(_ union _).distinct

  def vars = args.map(_.vars).reduce(_ union _).distinct

  def freeVars = args.map(_.freeVars).reduce(_ union _).distinct

  def boundVars = args.map(_.boundVars).reduce(_ union _).distinct

  def numOfAtoms = args.foldLeft(0)((sum, f) => sum + f.numOfAtoms)

  def numOfNodes = args.foldLeft(1)((sum, f) => sum + f.numOfNodes)

  def isNNF = args forall (_.isNNF)
}

/**
 * Companion object to simulate a case class
 */
object NAryOperator {
  def compactify[L <: Logic](Op: String, fs: Formula[L]*): List[Formula[L]] = {
    fs.foldLeft(Nil: List[Formula[L]])((list, elem) => elem match {
      case NAryOperator(Op, forms@_*) => forms.toList.reverse ++ list
      case _                          => elem :: list
    }).reverse
  }

  def unapplySeq[L <: Logic](f: Formula[L]) = f match {
    case n: NAryOperator[L] => Some((n.op, n.args))
    case _                  => None
  }
}
