package org.warthog.pl.optimization.maxSAT

import org.warthog.pl.datastructures.cnf.{ImmutablePLClause, PLLiteral}
import org.warthog.pl.datastructures.maxsat._

object ExampleInstances {
  val fs = System.getProperty("file.separator")

  val dir_formula = "test" + fs + "resources" + fs + "maxSAT"

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