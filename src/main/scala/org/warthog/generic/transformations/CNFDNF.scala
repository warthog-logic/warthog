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

package org.warthog.generic.transformations

import org.warthog.generic.formulas._

/**
 * Trait for mixing in CNF and DNF conversion and predicates
 *
 * Author: zengler
 * Date:   18.01.12
 */
trait CNFDNF[L <: Logic] extends Transformation[L] {

  /**
   * Is a formula in CNF
   * @return true if the formula is in CNF, false otherwise
   */
  def isCNF = cnfp(f)

  /**
   * Is a formula in DNF
   * @return true if the formula is in DNF, false otherwise
   */
  def isDNF = dnfp(f)

  /**
   * Get the CNF of a formula
   * @return the CNF of the formula
   */
  def cnf: Formula[L] = (-(computeDNF(-f))).nnf

  /**
   * Get the DNF of a formula
   * @return the DNF of the formula
   */
  def dnf: Formula[L] = computeDNF(f): Formula[L]

  /**
   * Get the simplified CNF of a formula
   * @return the simplified CNF of the formula
   */
  def simplifiedCNF: Formula[L] = simplifyCNF(cnf)

  /**
   * Get the simplified DNF of a formula
   * @return the simplified DNF of the formula
   */
  def simplifiedDNF: Formula[L] = simplifyDNF(dnf)

  private def cnfp(arg: Formula[L]): Boolean = arg match {
    case t: TruthValue[L]     => true
    case a: Atom[L]           => true
    case b: BinaryOperator[L] => false
    case Not(p)               => p match {
      case a: Atom[L] => true
      case _          => false
    }
    case And(fs@_*)           => fs.forall(cnfp(_))
    case Or(fs@_*)            => fs.forall(_.isLiteral)
  }

  private def dnfp(arg: Formula[L]): Boolean = arg match {
    case t: TruthValue[L]     => true
    case a: Atom[L]           => true
    case b: BinaryOperator[L] => false
    case Not(p)               => p match {
      case a: Atom[L] => true
      case _          => false
    }
    case And(fs@_*)           => fs.forall(_.isLiteral)
    case Or(fs@_*)            => fs.forall(dnfp(_))
  }

  private def computeDNF(arg: Formula[L]): Formula[L] = {
    def matdnf(p: Formula[L]): Formula[L] = p match {
      case Or(fs@_*)  => Or(fs.map(matdnf _): _*)
      case And(fs@_*) => fs.map(matdnf _).reduceLeft(distribute(_, _))
      case _          => p
    }
    matdnf(arg.nnf)
  }

  private def distribute(f0: Formula[L], f1: Formula[L]): Formula[L] = {
    (f0, f1) match {
      case (Or(fs0@_*), p) => Or(fs0.map(distribute(_, p)): _*)
      case (p, Or(fs0@_*)) => Or(fs0.map(distribute(_, p)): _*)
      case (p, q)          => And(p, q)
    }
  }

  private def simplifyCNF(f: Formula[L]): Formula[L] = f match {
    case And(fs@_*) => fs.map(simplifyCNF).filter(_ != Verum()).filterNot(x => fs.exists(y => isSubsumedBy(x, y))) match {
      case Nil => Verum()
      case l   => if (l.length == 1) l.head else And(l: _*)
    }
    case Or(fs@_*)  => if (fs.exists(x => fs.exists(y => y == -x))) Verum() else f
    case _          => f
  }

  /* true iff x is subsumed by y */
  private def isSubsumedBy(x: Formula[L], y: Formula[L]) = (x != y) && ((x, y) match {
    case (Or(xs@_*), Or(ys@_*)) => ys.toSet subsetOf xs.toSet
    case (Or(xs@_*), l)         => Set(l) subsetOf xs.toSet
    case _                      => false
  })

  private def simplifyDNF(f: Formula[L]): Formula[L] = f match {
    case Or(fs@_*)  => fs.map(simplifyDNF).filter(_ != Falsum())
      .filterNot(x => fs.exists(y => isAbsorbedBy(x, y))) match {
      case Nil => Falsum()
      case l   => if (l.length == 1) l.head else Or(l: _*)
    }
    case And(fs@_*) => if (fs.exists(x => fs.exists(y => y == -x))) Falsum() else f
    case _          => f
  }

  /* true iff x is absorbed by y */
  private def isAbsorbedBy(x: Formula[L], y: Formula[L]) = (x != y) && ((x, y) match {
    case (And(xs@_*), And(ys@_*)) => ys.toSet subsetOf xs.toSet
    case (And(xs@_*), l)          => Set(l) subsetOf xs.toSet
    case _                        => false
  })

}
