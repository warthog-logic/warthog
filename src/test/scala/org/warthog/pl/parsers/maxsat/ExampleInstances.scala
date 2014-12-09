package org.warthog.pl.parsers.maxsat

import org.warthog.pl.datastructures.cnf.PLLiteral

object ExampleInstances {
  val fs = System.getProperty("file.separator")

  val dir_formula = "src" + fs + "test" + fs + "resources" + fs + "maxSAT"

  // Simple formulas
  val dirPartialMaxSATSimple = dir_formula + fs + "partial" + fs + "simple"
  val dirPartialWeightedMaxSATSimple = dir_formula + fs + "partialWeighted" + fs + "simple"

  // Literals
  val (u, v, w, x, y, z) = (PLLiteral("u", true), PLLiteral("v", true), PLLiteral("w", true),
    PLLiteral("x", true), PLLiteral("y", true), PLLiteral("z", true))
  val (nu, nv, nw, nx, ny, nz) = (u.negate, v.negate, w.negate, x.negate, y.negate, z.negate)
}