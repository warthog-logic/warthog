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

package org.warthog.pl.transformations

import org.warthog.generic.transformations.Transformation
import org.warthog.pl.formulas.{PLAtom, PL}
import org.warthog.generic.formulas.{Or, NAryOperator, And, Formula}

/**
 * Implementation of definitional CNF
 *
 * Tseitin
 * Plaisted/Greenbaum
 */
trait DefinitionalCNF extends Transformation[PL] {

  private var auxilliaryCounter = 0L
  private val auxillaryPrefix = "CNFVar"

  def tseitinCNF: Formula[PL] = executeMethod(tseitin)

  def plaistedGreenbaumCNF: Formula[PL] = executeMethod(pg)

  private def executeMethod(method: Formula[PL] => (Option[PLAtom], List[Formula[PL]])) = {
    val formula = f.nnf.removeBooleanConstants
    if (formula.isCNF)
      formula
    else {
      val (k, v) = method(f.nnf.removeBooleanConstants)
      if (k != None)
        And((k.get :: v): _*)
      else
        v.head
    }
  }

  private def tseitin(fm: Formula[PL]): (Option[PLAtom], List[Formula[PL]]) = fm match {
    case m: NAryOperator[PL] => {
      val isAnd = m.op == Formula.AND
      val aux = newAuxillary
      var value = List[Formula[PL]]()
      val transformedChildren = transformChildren(m.args.toList, tseitin)
      val literals = if (isAnd)
        tpgLiteralsNegate(transformedChildren)
      else
        tpgLiterals(transformedChildren)
      val tempLit = if (isAnd) -aux else aux
      literals.foreach(l => value ::= Or(tempLit, -l))
      value ::= (if (isAnd) Or((aux :: literals): _*) else Or((-aux :: literals): _*))
      value = addFormulas(transformedChildren, value)
      (Some(aux), value)
    }
    case _                   => (None, List(fm))
  }

  private def pg(fm: Formula[PL]): (Option[PLAtom], List[Formula[PL]]) = fm match {
    case m: NAryOperator[PL] => {
      val isAnd = m.op == Formula.AND
      val aux = newAuxillary
      var value = List[Formula[PL]]()
      val transformedChildren = transformChildren(m.args.toList, pg)
      val literals = if (isAnd)
        tpgLiteralsNegate(transformedChildren)
      else
        tpgLiterals(transformedChildren)
      if (isAnd)
        literals.foreach(l => value ::= Or(-aux, -l))
      else
        value ::= Or((-aux :: literals): _*)
      value = addFormulas(transformedChildren, value)
      (Some(aux), value)
    }
    case _                   => (None, List(fm))
  }

  private def transformChildren(children: List[Formula[PL]], method: Formula[PL] => (Option[PLAtom], List[Formula[PL]])) =
    children.map(method)

  private def tpgLiterals(pairs: List[(Option[PLAtom], List[Formula[PL]])]) =
    pairs.map(e => (if (e._1 == None) e._2.head else e._1.get))

  private def tpgLiteralsNegate(pairs: List[(Option[PLAtom], List[Formula[PL]])]) =
    pairs.map(e => (if (e._1 == None) -e._2.head else -e._1.get))

  private def addFormulas(pairs: List[(Option[PLAtom], List[Formula[PL]])], list: List[Formula[PL]]) =
    pairs.foldLeft(list)((l, e) => (if (e._1 != None) e._2 ++ l else l))

  private def newAuxillary: PLAtom = {
    val p = PLAtom(auxillaryPrefix + auxilliaryCounter)
    auxilliaryCounter += 1
    p
  }
}
