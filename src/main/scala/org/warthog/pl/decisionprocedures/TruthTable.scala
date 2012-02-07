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

package org.warthog.pl.decisionprocedures

import org.warthog.pl.formulas.{PL, PLAtom}
import org.warthog.generic.formulas.Formula

/**
 * Propositional truth table generation
 *
 * Author: zengler
 * Date:   25.01.12
 */
object TruthTable {

  /**
   * Generate the truth table of a propositional formula
   * @param f a propositional formula
   * @return a stringrepresentation of f's truth table
   */
  def generate(f: Formula[PL]): String = {
    val sb = new StringBuilder
    val leftSideLength = f.vars.foldLeft(0)(_ + _.toString.length + 1)
    sb.append("-" * (leftSideLength + 10) + "\n")
    f.vars.addString(sb, " ")
    sb.append("  | formula\n")
    sb.append("-" * (leftSideLength + 10) + "\n")
    generateRows(sb, f, Map[PLAtom, Boolean](), f.vars.asInstanceOf[List[PLAtom]])
    sb.toString
  }

  private def generateRows(
                            sb: StringBuilder,
                            f: Formula[PL],
                            v: Map[PLAtom, Boolean],
                            ats: List[PLAtom]
                            ): Unit =
    ats match {
      case Nil     => getRow(sb, v, f.eval(v))
      case p :: ps => {
        generateRows(sb, f, v + (p -> false), ps)
        generateRows(sb, f, v + (p -> true), ps)
      }
    }

  private def getRow(sb: StringBuilder, v: Map[PLAtom, Boolean], r: Boolean): Unit = {
    def b2n(a: Boolean) = if (a) 1 else 0
    v.foreach({
      case (x, b) => sb.append(("%" + (x.toString.length) + "d ").format(b2n(b)))
    })
    sb.append(" | %7d\n".format(b2n(r)))
  }
}
