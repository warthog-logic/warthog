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

package org.warthog.pl.parsers.tptp

import util.parsing.combinator._
import org.warthog.pl.formulas.{PLFormula, PL, PLAtom}
import org.warthog.generic.formulas._
import org.warthog.generic.parsers.RunParser
import scala.language.implicitConversions

/**
  * A parser for a TPTP oriented syntax of propositional logic
  */
class TPTPPL(fm: String) {
  def pl: Formula[PL] = new PLParser().run(fm).getOrElse(throw new Exception("Parsing Error!")).f
}

class PLParser extends RegexParsers with PackratParsers with RunParser {
  implicit def folFormulaToFormulaFOL(fm: PLFormula): Formula[PL] = fm.f

  override type Elem = Char

  lazy val variable: PackratParser[PLFormula] = """([A-Za-z0-9_]+)""".r ^^ (v => new PLFormula(PLAtom(v)))

  lazy val const: PackratParser[PLFormula] = (Formula.TRUE | Formula.FALSE) ^^ {
    case Formula.TRUE  => new PLFormula(Verum())
    case Formula.FALSE => new PLFormula(Falsum())
  }

  lazy val literal: PackratParser[PLFormula] = ("~" ~ simp | simp) ^^ {
    case "~" ~ (n: PLFormula) => new PLFormula(-n)
    case n: PLFormula         => n
  }

  lazy val conj: PackratParser[PLFormula] = rep1sep(literal, Formula.AND) ^^ {
    _.reduceLeft(And(_, _))
  }

  lazy val disj: PackratParser[PLFormula] = rep1sep(conj, Formula.OR) ^^ {
    _.reduceLeft(Or(_, _))
  }

  lazy val simp: PackratParser[PLFormula] = variable ||| const ||| (Formula.PARENL ~ expr ~ Formula.PARENR ^^ {
    case Formula.PARENL ~ e ~ Formula.PARENR => e
  })

  lazy val impl: PackratParser[PLFormula] = (disj ~ Formula.IMPL ~ impl ||| disj ~ Formula.IMPLR ~ impl ||| disj) ^^ {
    case (e0: PLFormula) ~ Formula.IMPL ~  (e1: PLFormula) => new PLFormula(Implication(e0, e1))
    case (e0: PLFormula) ~ Formula.IMPLR ~ (e1: PLFormula) => new PLFormula(Implication(e1, e0))
    case d: PLFormula                                      => d
  }

  lazy val equiv: PackratParser[PLFormula] = (impl ~ Formula.EQUIV ~ equiv ||| impl ~ Formula.XOR ~ equiv ||| impl) ^^ {
    case (e0: PLFormula) ~ Formula.EQUIV ~ (e1: PLFormula) => new PLFormula(Equiv(e0, e1))
    case (e0: PLFormula) ~ Formula.XOR   ~ (e1: PLFormula) => new PLFormula(Xor(e0, e1))
    case d: PLFormula                                      => d
  }

  lazy val expr: PackratParser[PLFormula] = equiv

  type RootType = PLFormula

  def root = expr
}
