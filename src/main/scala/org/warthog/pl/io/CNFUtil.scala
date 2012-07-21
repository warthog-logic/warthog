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

package org.warthog.pl.io

import org.warthog.pl.formulas.{ PLAtom, PL }
import org.warthog.generic.formulas._
import org.warthog.pl.datastructures.cnf.PLClause

/**
  * A utility class to produce different output formats for CNFs
  */
object CNFUtil {
  /**
    * List representation of a CNF formula
    * @param f a formula in cnf
    * @return a list (conjunction) of lists (clauses) of literals
    */
  def toList(f: Formula[PL]): List[List[Formula[PL]]] = {
    (f.cnf match {
      case And(fs@_*) => (for (i <- fs; x = toList(i); if x != Nil) yield x.head).toList
      case Or(fs@_*)  => List(fs.toList.filter(!_.isInstanceOf[Falsum[PL]]))
      case literal    => List(if (literal.isInstanceOf[Falsum[PL]]) Nil else List(literal))
    }).filterNot(_.contains(Verum[PL]()))
  }

  /**
    * Internal CNF representation with clauses
    * @param f a formula in cnf
    * @return a list of clauses
    */
  def toCNF(f: Formula[PL]): List[PLClause] = {
    (f.simplifiedCNF.removeBooleanConstants match {
      case v: Verum[PL]  => List[PLClause]()
      case f: Falsum[PL] => List(new PLClause())
      case And(fs@_*)    => (for (i <- fs; x = toCNF(i)) yield x.head).toList
      case Or(fs@_*)     => List(new PLClause(fs.toList.asInstanceOf[List[PLAtom]]))
      case lit: PLAtom   => List(new PLClause(lit))
    })
  }
}
