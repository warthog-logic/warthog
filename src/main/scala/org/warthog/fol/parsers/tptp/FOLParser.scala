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

package org.warthog.fol.parsers.tptp

import util.parsing.combinator.{ PackratParsers, RegexParsers }
import org.warthog.fol.formulas._
import org.warthog.generic.formulas._
import org.warthog.generic.parsers.RunParser
import scala.language.implicitConversions

/**
  * A parser for TPTP
  */
class TPTPFOL(fm: String) {
  def fol: Formula[FOL] = new FOLParser().run(fm).getOrElse(throw new Exception("Parsing Error!")).f
}

class FOLParser extends RegexParsers with PackratParsers with RunParser {
  implicit def folFormulaToFormulaFOL(fm: FOLFormula): Formula[FOL] = fm.f

  override type Elem = Char

  val allQuant = "!"
  val exQuant = "?"

  def infixPred: Parser[String] = """[=<>]""".r

  def definedProp: PackratParser[FOLFormula] = ("$true" | "$false") ^^ {
    case "$true"  => new FOLFormula(Verum())
    case "$false" => new FOLFormula(Falsum())
  }

  def variable: PackratParser[FOLVariable] = """[A-Z][A-Za-z0-9_]*""".r ^^ (FOLVariable(_))

  def termName: Parser[String] = """[a-z][A-Za-z0-9_]*""".r

  def number: PackratParser[FOLTerm] =
    """[0-9]+(.[0-9]+)?""".r ^^ { case x: String => FOLFunction(BigDecimal(x).toString()) }

  def term: PackratParser[FOLTerm] =
    variable |||
      number |||
      termName ~ opt("(" ~> rep1sep(term, ",") <~ ")") ^^ {
        case functor ~ Some(terms) => FOLFunction(functor, terms: _*)
        case functor ~ _           => FOLFunction(functor)
      }

  def predicate: PackratParser[FOLFormula] =
    term ~ infixPred ~ term ^^ { case tm1 ~ inf ~ tm2 => new FOLFormula(FOLPredicate(inf, tm1, tm2)) } |
      termName ~ opt("(" ~> rep1sep(term, ",") <~ ")") ^^ {
        case functor ~ Some(terms) => new FOLFormula(FOLPredicate(functor, terms: _*))
        case functor ~ _           => new FOLFormula(FOLPredicate(functor))
      }

  def variableList: PackratParser[List[FOLVariable]] =
    "[" ~> rep1sep(variable, ",") <~ "]"

  def quantifier: PackratParser[String] = allQuant | exQuant

  def quantifiedFormula: PackratParser[FOLFormula] = quantifier ~ variableList ~ ":" ~ formula ^^ {
    case `allQuant` ~ variables ~ ":" ~ fm => new FOLFormula(FOLForAll(Set(variables: _*), fm))
    case `exQuant` ~ variables ~ ":" ~ fm  => new FOLFormula(FOLExists(Set(variables: _*), fm))
  }

  lazy val literal: PackratParser[FOLFormula] =
    ("~" ~ simp | simp) ^^ { case "~" ~ (n: FOLFormula) => new FOLFormula(-n); case n: FOLFormula => n }

  lazy val conj: PackratParser[FOLFormula] = rep1sep(literal, "&") ^^ { _.reduceLeft(And(_, _)) }

  lazy val disj: PackratParser[FOLFormula] = rep1sep(conj, "|") ^^ { _.reduceLeft(Or(_, _)) }

  lazy val simp: PackratParser[FOLFormula] =
    predicate |||
      definedProp |||
      quantifiedFormula |||
      ("(" ~ formula ~ ")" ^^ { case "(" ~ e ~ ")" => e })

  lazy val impl: PackratParser[FOLFormula] = (disj ~ "=>" ~ impl ||| disj ~ "<=" ~ impl ||| disj) ^^ {
    case (e0: FOLFormula) ~ "=>" ~ (e1: FOLFormula) => new FOLFormula(Implication(e0, e1))
    case (e0: FOLFormula) ~ "<=" ~ (e1: FOLFormula) => new FOLFormula(Implication(e1, e0))
    case d: FOLFormula                                => d
  }

  lazy val equiv: PackratParser[FOLFormula] = (impl ~ "<=>" ~ equiv ||| impl ~ "<~>" ~ equiv ||| impl) ^^ {
    case (e0: FOLFormula) ~ "<=>" ~ (e1: FOLFormula) => new FOLFormula(Equiv(e0, e1))
    case (e0: FOLFormula) ~ "<~>" ~ (e1: FOLFormula) => new FOLFormula(Xor(e0, e1))
    case d: FOLFormula                                 => d
  }

  lazy val formula: PackratParser[FOLFormula] = equiv

  type RootType = FOLFormula

  def root = formula
}
