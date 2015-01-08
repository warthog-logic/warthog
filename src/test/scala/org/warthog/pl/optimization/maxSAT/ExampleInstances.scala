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

package org.warthog.pl.optimization.maxsat

import org.warthog.pl.datastructures.cnf.{ImmutablePLClause, PLLiteral}

object ExampleInstances {
  val fs = System.getProperty("file.separator")

  val dir_formula = "src" + fs + "test" + fs + "resources" + fs + "maxSAT"

  // Simple formulas
  val dirPartialMaxSATSimple = dir_formula + fs + "partial" + fs + "simple"
  val dirPartialWeightedMaxSATSimple = dir_formula + fs + "partialWeighted" + fs + "simple"

  // Random formulas
  // val dirPartialMaxSATRandomMaxSAT = dir_formula + fs + "partial" + fs + "random"
  // val dir_PWMaxSAT_randMaxSAT = dir_formula + fs + "partialWeighted" + fs + "random"

  // Random Vertex Cover formulas
  // val dir_PMaxSAT_randVertexCover = dir_formula + fs + "partialWeighted" + fs + "randomVertexCover"

  // Literals
  val (u, v, w, x, y, z) = (PLLiteral("u", true), PLLiteral("v", true), PLLiteral("w", true),
    PLLiteral("x", true), PLLiteral("y", true), PLLiteral("z", true))
  val (nu, nv, nw, nx, ny, nz) = (u.negate, v.negate, w.negate, x.negate, y.negate, z.negate)
}