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

package pl.generators

import org.warthog.pl.decisionprocedures.satsolver.impl.picosat.Picosat
import org.warthog.pl.decisionprocedures.satsolver.{Infinity, sat}
import org.warthog.generic.formulas.Formula
import org.specs2.mutable.Specification
import org.warthog.pl.formulas.{PL, PLAtom}
import org.warthog.pl.generators.cardinality._

/**
 * Tests for the cardinality constraints
 *
 * Author: kuebler
 * Date:   25.01.12
 */
class CardinalityConstraintTest extends Specification {
  val ps = new Picosat
  val (w, x, y, z) = (PLAtom("w"), PLAtom("x"), PLAtom("y"), PLAtom("z"))
  var rv: Int = _

  args(sequential = true)

  lefrag("x+y+z <= 2 (Bailleux-Boufkhad)", BailleuxBoufkhad.le(Array(x, y, z), 2))
  lefrag("x+y+z <= 2 (Bitonic Sorting)", BitonicSorting.le(Array(x, y, z), 2))

  ltfrag("x+y+z < 2 (Bailleux-Boufkhad)", BailleuxBoufkhad.lt(Array(x, y, z), 2))
  ltfrag("x+y+z < 2 (Bitonic Sorting)", BitonicSorting.lt(Array(x, y, z), 2))

  eq3t2frag("x+y+z == 2 (Bailleux-Boufkhad)", BailleuxBoufkhad.eq(Array(x, y, z), 2))
  eq3t2frag("x+y+z == 2 (Bitonic Sorting)", BitonicSorting.eq(Array(x, y, z), 2))

  eq4t2frag("w+x+y+z == 2 (Bailleux-Boufkhad)", BailleuxBoufkhad.eq(Array(w, x, y, z), 2))
  eq4t2frag("w+x+y+z == 2 (Bitonic Sorting)", BitonicSorting.eq(Array(w, x, y, z), 2))

  gefrag("x+y+z >= 2 (Bailleux-Boufkhad)", BailleuxBoufkhad.ge(Array(x, y, z), 2))
  gefrag("x+y+z >= 2 (Bitonic Sorting)", BitonicSorting.ge(Array(x, y, z), 2))

  gtfrag("x+y+z > 2 (Bailleux-Boufkhad)", BailleuxBoufkhad.gt(Array(x, y, z), 2))
  gtfrag("x+y+z > 2 (Bitonic Sorting)", BitonicSorting.gt(Array(x, y, z), 2))

  le4t2frag("w+x+y+z <= 2 (Bailleux-Boufkhad)", BailleuxBoufkhad.le(Array(w, x, y, z), 2))
  le4t2frag("w+x+y+z <= 2 (Bitonic Sorting)", BitonicSorting.le(Array(w, x, y, z), 2))

  lt4t2frag("w+x+y+z <= 2 (Bailleux-Boufkhad)", BailleuxBoufkhad.lt(Array(w, x, y, z), 2))
  lt4t2frag("w+x+y+z <= 2 (Bitonic Sorting)", BitonicSorting.lt(Array(w, x, y, z), 2))

  ge4t2frag("w+x+y+z >= 2 (Bailleux-Boufkhad)", BailleuxBoufkhad.ge(Array(w, x, y, z), 2))
  ge4t2frag("w+x+y+z >= 2 (Bitonic Sorting)", BitonicSorting.ge(Array(w, x, y, z), 2))

  gt4t2frag("w+x+y+z > 2 (Bailleux-Boufkhad)", BailleuxBoufkhad.gt(Array(w, x, y, z), 2))
  gt4t2frag("w+x+y+z > 2 (Bitonic Sorting)", BitonicSorting.gt(Array(w, x, y, z), 2))

  def lefrag(name: String, fm: Formula[PL]) = name should {
    "be satisfiable" in {
      sat(ps) {
        s => {
          s.add(fm)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo (1)
    }
    "be unsatisfiable after adding x=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm)
          s.add(x && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo (-1)
    }
    "be satisfiable for x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm)
          s.add(-x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo (1)
    }
  }

  def ltfrag(name: String, fm: Formula[PL]) = name should {
    "be satisfiable" in {
      sat(ps) {
        s => {
          s.add(fm)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=true" in {
      sat(ps) {
        s => {
          s.add(fm && x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=true" in {
      sat(ps) {
        s => {
          s.add(fm && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for z=true" in {
      sat(ps) {
        s => {
          s.add(fm && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be unsatisfiable for x=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
  }

  def eq3t2frag(name: String, fm: Formula[PL]) = name should {
    "be satisfiable" in {
      sat(ps) {
        s => {
          s.add(fm)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be unsatisfiable for x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
  }

  def eq4t2frag(name: String, fm: Formula[PL]) = name should {
    "be satisfiable" in {
      sat(ps) {
        s => {
          s.add(fm)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be unsatisfiable for w=x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=x=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=x=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
  }

  /* x+y+z >= 2 */
  def gefrag(name: String, fm: Formula[PL]) = name should {
    "be satisfiable" in {
      sat(ps) {
        s => {
          s.add(fm)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=true" in {
      sat(ps) {
        s => {
          s.add(fm && x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=true" in {
      sat(ps) {
        s => {
          s.add(fm && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for z=true" in {
      sat(ps) {
        s => {
          s.add(fm && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be unsatisfiable for x=y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
  }

  def gtfrag(name: String, fm: Formula[PL]) = name should {
    "be satisfiable" in {
      sat(ps) {
        s => {
          s.add(fm)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=true" in {
      sat(ps) {
        s => {
          s.add(fm && x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=true" in {
      sat(ps) {
        s => {
          s.add(fm && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for z=true" in {
      sat(ps) {
        s => {
          s.add(fm && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be unsatisfiable for x=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
  }

  /* w+x+y+z <= 2 */
  def le4t2frag(name: String, fm: Formula[PL]) = name should {
    "be satisfiable" in {
      sat(ps) {
        s => {
          s.add(fm)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=true" in {
      sat(ps) {
        s => {
          s.add(fm && w)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=true" in {
      sat(ps) {
        s => {
          s.add(fm && x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=true" in {
      sat(ps) {
        s => {
          s.add(fm && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for z=true" in {
      sat(ps) {
        s => {
          s.add(fm && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be unsatisfiable for w=x=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=x=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=x=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be satisfiable for w=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
  }

  /* w+x+y+z < 2 */
  def lt4t2frag(name: String, fm: Formula[PL]) = name should {
    "be satisfiable" in {
      sat(ps) {
        s => {
          s.add(fm)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=true" in {
      sat(ps) {
        s => {
          s.add(fm && w)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=true" in {
      sat(ps) {
        s => {
          s.add(fm && x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=true" in {
      sat(ps) {
        s => {
          s.add(fm && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for z=true" in {
      sat(ps) {
        s => {
          s.add(fm && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be unsatisfiable for w=x=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=x=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=x=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=x=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be satisfiable for w=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
  }

  /* w+x+y+z >= 2 */
  def ge4t2frag(name: String, fm: Formula[PL]) = name should {
    "be satisfiable" in {
      sat(ps) {
        s => {
          s.add(fm)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=true" in {
      sat(ps) {
        s => {
          s.add(fm && w)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=true" in {
      sat(ps) {
        s => {
          s.add(fm && x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=true" in {
      sat(ps) {
        s => {
          s.add(fm && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for z=true" in {
      sat(ps) {
        s => {
          s.add(fm && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be unsatisfiable for w=x=y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=x=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
  }

  /* w+x+y+z > 2 */
  def gt4t2frag(name: String, fm: Formula[PL]) = name should {
    "be satisfiable" in {
      sat(ps) {
        s => {
          s.add(fm)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=true" in {
      sat(ps) {
        s => {
          s.add(fm && w)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=true" in {
      sat(ps) {
        s => {
          s.add(fm && x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=true" in {
      sat(ps) {
        s => {
          s.add(fm && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for z=true" in {
      sat(ps) {
        s => {
          s.add(fm && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=y=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x && y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && x && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=x=y=z=true" in {
      sat(ps) {
        s => {
          s.add(fm && w && x && y && z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for w=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for x=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be satisfiable for z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo 1
    }
    "be unsatisfiable for w=x=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=x=y=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x && -y)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=x=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
    "be unsatisfiable for w=x=y=z=false" in {
      sat(ps) {
        s => {
          s.add(fm && -w && -x && -y && -z)
          rv = s.sat(Infinity)
        }
      }
      rv must be equalTo -1
    }
  }
}
