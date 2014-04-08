/*
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

package org.warthog.pl.parsers.tptp

import util.parsing.combinator._
import org.warthog.generic.parsers.RunParser
import org.warthog.pl.knowledgecompilation.dnnf._

/**
 * A parser for a TPTP oriented syntax of propositional logic
 */
class TPTPDNNF(fm: String) {
  def dnnf: DNNF = new DNNFParser().run(fm).getOrElse(throw new Exception("Parsing Error!"))
}

class DNNFParser extends RegexParsers with PackratParsers with RunParser {
  override type Elem = Char

  lazy val variable: PackratParser[String] = """([A-Za-z0-9]+)""".r ^^ {
    case v => v
  }

  lazy val const: PackratParser[DNNF] = ("$true" | "$false") ^^ {
    case "$true"  => True
    case "$false" => False
  }

  lazy val literal: PackratParser[DNNF] = ("~" ~ variable | variable) ^^ {
    case "~" ~ (v: String) => StringLit(v, false)
    case v: String         => StringLit(v, true)
  } ||| const ||| "(" ~ expr ~ ")" ^^ {
    case "(" ~ e ~ ")" => e
  }

  lazy val conj: PackratParser[DNNF] = rep1sep(literal, "&") ^^ {
    lits => lits.length match {
      case 1 => lits.head
      case _ => And(lits)
    }
  }

  lazy val disj: PackratParser[DNNF] = rep1sep(conj, "|") ^^ {
    ands => ands.length match {
      case 1 => ands.head
      case _ => Or(ands)
    }
  }

  lazy val expr: PackratParser[DNNF] = disj

  type RootType = DNNF

  def root = expr
}
