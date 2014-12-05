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

package org.warthog.pl.algorithms

import org.warthog.pl.formulas.{PL, PLAtom}
import org.warthog.generic.formulas.{Formula, Verum, Falsum}

/**
 * Propositional Craig Interpolation.
 *
 * Description:
 * Let phi and psi two propositional formulas such that phi => psi holds.
 * A Craig Interpolant of phi and psi is a propositional formula rho such that
 * a) phi => rho  and
 * b) rho => psi  and
 * c) vars(rho) = vars(phi) intersection vars(psi).
 */
object CraigInterpolation {
  /**
   * Compute the craig interpolant of two propositional formulas
   * @param p the first formula
   * @param q the second formula
   * @return the craig interpolant of p and q
   */
  def pinterpolate(p: Formula[PL], q: Formula[PL]): Formula[PL] = {
    val setMinus = p.vars.filterNot(q.vars.contains(_)).asInstanceOf[List[PLAtom]]
    setMinus.foldLeft(p)((expandedP, v) =>
      expandedP.substitute(v, Falsum()).removeBooleanConstants ||
        expandedP.substitute(v, Verum()).removeBooleanConstants).removeBooleanConstants
  }
}