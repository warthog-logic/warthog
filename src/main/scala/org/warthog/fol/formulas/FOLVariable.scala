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

import org.warthog.generic.formulas.{Formula, Variable}

/**
 * A FOL individual variable
 * @param name the name of the variable
 *
 * Author: zengler
 * Date:   25.01.12
 */
case class FOLVariable(name: String) extends FOLTerm with Variable[FOL] {

  override def toString = name

  def vars = List(this.asInstanceOf[Variable[FOL]])

  def functions = List()

  def numOfNodes = 1

  def tsubst(s: Map[FOLVariable, FOLTerm]) = s.getOrElse(this, this)

  /**
   * Generate a fresh variable which is not present in a given formula.
   * @param f the formula
   * @param s an additional set of disallowed variables
   * @param i the current variable index (should not be set by the user)
   *
   * @return a fresh variable
   */
  def freshVariable(f: Formula[FOL], s: Set[FOLVariable] = Set[FOLVariable](), i: Int = 0): FOLVariable = {
    val varParser = """(.*?)(\d*)""".r
    val varParser(prefix, number) = name
    val newVar = if (number.isEmpty) FOLVariable(prefix + i) else FOLVariable(prefix + (Integer.parseInt(number) + 1))
    if (!f.vars.contains(newVar) && !(s.contains(newVar)))
      newVar
    else
      freshVariable(f, s, i + 1)
  }
}
