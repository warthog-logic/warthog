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

package org.warthog.generic.printer

import org.warthog.generic.formulas._

/**
 * A UTF-8 printer for formulas
 *
 * Author: zengler
 * Date:   01.02.12
 */
class UTF8Printer[-L <: Logic] extends PrettyPrinter[L] {
  def print[T <: L](f: Formula[T]) = f match {
    case v: Verum[T]          => Formula.TRUE
    case f: Falsum[T]         => Formula.FALSE
    case Not(p)               => Formula.NOT + (if (f.priority < p.priority) print(p) else "(" + print(p) + ")")
    case p: BinaryOperator[T] => (if (f.priority < p.f1.priority) print(p.f1) else "(" + print(p.f1) + ")") + " " + p.op + " " + (if (f.priority < p.f2.priority) print(p.f2) else "(" + print(p.f2) + ")")
    case p: NAryOperator[T]   => (p.args.map(x => if (f.priority < x.priority) print(x) else "(" + print(x) + ")") mkString (" " + p.op + " "))
  }
}

object UTF8Printer {
  val CONST = "\u2071"
  val PREDCONST = "\u2070"

  def prettyPrintName(name: String) = {
    val varParser = """(.*?)(\d*)""".r
    val varParser(prefix, number) = name
    if (number.isEmpty)
      prefix
    else
      prefix + getSubscript(number)
  }

  private def getSubscript(i: String) = {
    def matchSingle(i: Char) = i match {
      case '0' => "\u2080"
      case '1' => "\u2081"
      case '2' => "\u2082"
      case '3' => "\u2083"
      case '4' => "\u2084"
      case '5' => "\u2085"
      case '6' => "\u2086"
      case '7' => "\u2087"
      case '8' => "\u2088"
      case '9' => "\u2089"
    }
    i.toSeq.foldLeft("")((l,e) => l + matchSingle(e))
  }
}
