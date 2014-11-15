/*
 * Copyright (c) 2011-2014, Andreas J. Kuebler & Christoph Zengler & Rouven Walter
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

package org.warthog.pl.optimization.maxSAT.partialWeighted

import org.warthog.generic.formulas.Formula
import org.warthog.pl.formulas.PL
import org.warthog.pl.datastructures.cnf.ImmutablePLClause
import org.warthog.pl.optimization.maxSAT.MaxSATHelper
import collection.mutable.{ListBuffer, MutableList}

/**
 * Common interface for Partial Weighted MaxSAT solvers.
 */
abstract class PartialWeightedMaxSATSolver {

  protected var hardClauses = new ListBuffer[ImmutablePLClause]
  protected var softClauses = new ListBuffer[ImmutablePLClause]
  protected var weights = new ListBuffer[Long]
  protected var minUNSATResult: Option[Long] = None

  def name()

  def reset() {
    hardClauses.clear()
    softClauses.clear()
    weights.clear()
    minUNSATResult = None
  }

  def addHardClause(clause: ImmutablePLClause) {
    hardClauses += clause
  }

  def addSoftClause(clause: ImmutablePLClause, weight: Long) {
    softClauses += clause
    weights += weight
  }

  protected def solveMinUNSATImpl(): Option[Long]

  def solveMinUNSAT() = {
    if (!MaxSATHelper.isSAT(hardClauses))
      None
    else
      solveMinUNSATImpl()
  }
}