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

package org.warthog.fol.printer

import org.warthog.generic.printer.{ UTF8Printer => SuperPrinter }
import org.warthog.generic.formulas.{ Quantifier, Formula }
import org.warthog.fol.formulas._

/**
  * An UTF8 printer for first order logic formulas
  */
object UTF8Printer extends SuperPrinter[FOL] {
  override def print[T <: FOL](f: Formula[T]) = f match {
    case p: Quantifier[_] => {
      val form = if (p.priority == p.arg.priority)
        "%s%s".format(printTerm(p.x.asInstanceOf[FOLTerm]), print(p.arg))
      else
        "%s: %s".format(printTerm(p.x.asInstanceOf[FOLTerm]), print(p.arg))
      SuperPrinter.ppQuantor(p.quant) + form
    }
    case FOLPredicate(s, args@_*) => {
      if (args.size == 0)
        SuperPrinter.ppName(s.name) + SuperPrinter.PREDCONST
      else if (args.size == 2 && s.name.size == 1 && "<>=".contains(s.name))
        printTerm(args(0)) + " " + s + " " + printTerm(args(1))
      else
        SuperPrinter.ppName(s.name) + "(" + args.map(x => printTerm(x)).mkString(",") + ")"
    }
    case p: Formula[T] => super.print(p)
  }

  private def printTerm(t: FOLTerm): String = t match {
    case FOLVariable(n) => SuperPrinter.ppName(n)
    case FOLFunction(s, args@_*) => {
      if (args.size == 0)
        s.name + SuperPrinter.CONST
      else
        SuperPrinter.ppName(s.name) + "(" + args.map(x => printTerm(x)).mkString(",") + ")"
    }
  }
}

