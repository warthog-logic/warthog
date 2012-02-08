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

package org.warthog.pl.parsers

import util.parsing.combinator._
import org.warthog.pl.formulas.{PL, PLAtom}
import org.warthog.generic.formulas._
import org.warthog.generic.parsers.RunParser

/**
 * A parser for a TPTP oriented syntax of propositional logic
 *
 * Author: kuebler
 * Date:   25.01.12
 */
class TPTPPL(fm: String) {
  def pl: Formula[PL] = new PLParser().run(fm).getOrElse(throw new Exception("Parsing Error!"))
}

class PLParser extends RegexParsers with PackratParsers with RunParser {
  override type Elem = Char
  lazy val variable: PackratParser[Formula[PL]] = """([A-Za-z0-9]+)""".r ^^ (PLAtom(_))
  lazy val const: PackratParser[Formula[PL]] = ("$true" | "$false") ^^ {
    case "$true" => Verum();
    case "$false" => Falsum()
  }
  lazy val literal: PackratParser[Formula[PL]] = ("~" ~ simp | simp) ^^ {
    case "~" ~ (n: Formula[PL]) => -n;
    case n: Formula[PL] => n
  }
  lazy val conj: PackratParser[Formula[PL]] = rep1sep(literal, "&") ^^ {
    _.reduceLeft(And(_, _))
  }
  lazy val disj: PackratParser[Formula[PL]] = rep1sep(conj, "|") ^^ {
    _.reduceLeft(Or(_, _))
  }
  lazy val simp: PackratParser[Formula[PL]] = variable ||| const ||| ("(" ~ expr ~ ")" ^^ {
    case "(" ~ e ~ ")" => e
  })
  lazy val impl: PackratParser[Formula[PL]] = (disj ~ "=>" ~ impl ||| disj ~ "<=" ~ impl ||| disj) ^^ {
    case (e0: Formula[PL]) ~ "=>" ~ (e1: Formula[PL]) => Implication(e0, e1)
    case (e0: Formula[PL]) ~ "<=" ~ (e1: Formula[PL]) => Implication(e1, e0)
    case d: Formula[PL]                               => d
  }
  lazy val equiv: PackratParser[Formula[PL]] = (impl ~ "<=>" ~ equiv ||| impl ~ "<~>" ~ equiv ||| impl) ^^ {
    case (e0: Formula[PL]) ~ "<=>" ~ (e1: Formula[PL]) => Equiv(e0, e1)
    case (e0: Formula[PL]) ~ "<~>" ~ (e1: Formula[PL]) => Xor(e0, e1)
    case d: Formula[PL]                                => d
  }
  lazy val expr: PackratParser[Formula[PL]] = equiv

  type RootType = Formula[PL]

  def root = expr
}
