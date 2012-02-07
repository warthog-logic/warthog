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

package fol.parsers

import org.specs2.mutable._
import org.warthog.fol.parsers.tptp._
import fol.F
import org.warthog.fol.formulas._
import org.warthog.generic.formulas._

/**
 * Test cases for the propositional logic parsers
 *
 * Author: zengler
 * Date:   01.02.12
 */
class TPTPParserTest extends Specification {
  val verum = Verum()
  val falsum = Falsum()
  val x = FOLPredicate("x")
  val y = FOLPredicate("y")
  val z = FOLPredicate("z")
  val x_impl_y = Implication(x, y)
  val y_impl_x = Implication(y, x)
  val x_equiv_y = Equiv(x, y)
  val x_xor_y = Xor(x, y)
  val xy = x && y
  val xyz = x && y && z
  val xoy = x || y
  val xoyoz = x || y || z
  val xoyz = x || (y && z)
  val xyoz = (x && y) || z
  val impl1 = Implication(x || -y, -z && y)
  val impl2 = Implication(x, Implication(-y, -z))
  val impl1vv = Implication(-z && y, x || -y)
  val equiv1 = Equiv(Implication(x, -y), Implication(y, -z))
  val equiv2 = Equiv(x, Equiv(-y, -z))
  val xor1 = Xor(Implication(x, -y), Implication(y, -z))
  val xor2 = Xor(x, Xor(-y, -z))
  val xorequiv1 = Xor(x, Equiv(-y, -z))
  val equivxor1 = Equiv(x, Xor(-y, -z))

  val xoyz_br = (x || y) && z
  val xyoz_br = x && (y || z)
  val impl1_br = (x || Implication(-y, -z)) && y
  val equiv1_br = Implication(y, Implication(x, Equiv(-y, -z)))
  val xor1_br = Implication(y, Implication(x, Xor(-y, -z)))
  val xorequiv1_br = Equiv(Xor(x, -y), -z)
  val equivxor1_br = Xor(Equiv(x, -y), -z)

  val X = FOLVariable("X")
  val Y = FOLVariable("Y")
  val Z = FOLVariable("Z")
  val W = FOLVariable("W")
  val U = FOLVariable("U")
  val V = FOLVariable("V")
  val px = FOLPredicate("p", X)
  val pxy = FOLPredicate("p", X, Y)
  val pxyz = FOLPredicate("p", X, Y, Z)
  val pxyzw = FOLPredicate("p", X, Y, Z, W)

  val P = FOLVariable("P")
  val Q = FOLVariable("Q")
  val a = FOLFunction("a")
  val b = FOLFunction("b")
  val pab = FOLPredicate("p", a, b)
  val pfp = FOLPredicate("p", FOLFunction("f", P))
  val pfpgq = FOLPredicate("p", FOLFunction("f", P), FOLFunction("g", Q))
  val pfpgqab = FOLPredicate("p", FOLFunction("f", P), FOLFunction("g", Q), a, b)
  val pfpagqb = FOLPredicate("p", FOLFunction("f", P, a), FOLFunction("g", Q, b))

  val pp = FOLPredicate("p")
  val axp = FOLForAll(X, pp)
  val axyzp = FOLForAll(Set(X, Y, Z), pp)
  val exp = FOLExists(X, pp)
  val exyzp = FOLExists(Set(X, Y, Z), pp)

  val h144 = Implication(FOLForAll(X, FOLPredicate("p", X) || FOLPredicate("r", Y)), FOLExists(Set(Y, Z), FOLPredicate("q", Y) || -FOLExists(Z, FOLPredicate("p", Z) && FOLPredicate("q", Z))))
  val h150_1 = FOLExists(Y, Implication(FOLPredicate("<", X, Y), FOLForAll(U, FOLExists(V, FOLPredicate("<", FOLFunction("mul", X, U), FOLFunction("mul", Y, V))))))
  val h150_2 = FOLForAll(X, Implication(FOLPredicate("p", X), (FOLExists(Set(X, Y), FOLPredicate("q", Y)) || -FOLExists(Z, FOLPredicate("p", Z) && FOLPredicate("q", Z)))))

  F.verum should {
    "be parsed to " + verum in {
      F.verum.fol must be equalTo verum
    }
  }

  F.falsum should {
    "be parsed to " + falsum in {
      F.falsum.fol must be equalTo falsum
    }
  }

  F.x should {
    "be parsed to " + x in {
      F.x.fol must be equalTo x
    }
  }

  F.y should {
    "be parsed to " + y in {
      F.y.fol must be equalTo y
    }
  }

  F.notx should {
    "be parsed to " + -x in {
      F.notx.fol must be equalTo -x
    }
  }

  F.noty should {
    "be parsed to " + -y in {
      F.noty.fol must be equalTo -y
    }
  }

  F.x_impl_y should {
    "be parsed to " + x_impl_y in {
      F.x_impl_y.fol must be equalTo x_impl_y
    }
  }

  F.y_impl_x should {
    "be parsed to " + y_impl_x in {
      F.y_impl_x.fol must be equalTo y_impl_x
    }
    "be equal to y => x" in {
      F.y_impl_x.fol must be equalTo "y => x".fol
    }
  }

  F.x_equiv_y should {
    "be parsed to " + x_equiv_y in {
      F.x_equiv_y.fol must be equalTo x_equiv_y
    }
  }

  F.x_xor_y should {
    "be parsed to " + x_xor_y in {
      F.x_xor_y.fol must be equalTo x_xor_y
    }
  }

  F.xy should {
    "be parsed to " + xy in {
      F.xy.fol must be equalTo xy
    }
  }

  F.xyz should {
    "be parsed to " + xyz in {
      F.xyz.fol must be equalTo xyz
    }
  }

  F.xoy should {
    "be parsed to " + xoy in {
      F.xoy.fol must be equalTo xoy
    }
  }

  F.xoyoz should {
    "be parsed to " + xoyoz in {
      F.xoyoz.fol must be equalTo xoyoz
    }
  }

  "Precedences of - and &" should {
    "be " + (-x && z) + " for ~x & z" in {
      "~x & z".fol must be equalTo -x && z
    }
    "be " + (x && -z) + " for x & ~z" in {
      "x & ~z".fol must be equalTo x && -z
    }
    "be " + (-x && -z) + " for ~x & ~z" in {
      "~x & ~z".fol must be equalTo -x && -z
    }
  }

  "Precedences of - and |" should {
    "be " + (-x || z) + " for ~x | z" in {
      "~x | z".fol must be equalTo -x || z
    }
    "be " + (x || -z) + " for x | ~z" in {
      "x | ~z".fol must be equalTo x || -z
    }
    "be " + (-x || -z) + " for ~x | ~z" in {
      "~x | ~z".fol must be equalTo -x || -z
    }
  }

  "Precedences of & and |" should {
    "be " + xoyz + " for x | y & z" in {
      F.xoyz.fol must be equalTo xoyz
    }
    "be " + xyoz + " for x & y | z" in {
      F.xyoz.fol must be equalTo xyoz
    }
  }

  "Precedences of &,| and =>" should {
    "be " + impl1 + " for " + F.impl1 in {
      F.impl1.fol must be equalTo impl1
    }
    "be " + impl1vv + " for " + F.impl1vv in {
      F.impl1vv.fol must be equalTo impl1vv
    }
  }

  "Precedences of =>, <=> and <~>" should {
    "be " + equiv1 + " for " + F.equiv1 in {
      F.equiv1.fol must be equalTo equiv1
    }
    "be " + xor1 + " for " + F.xor1 in {
      F.xor1.fol must be equalTo xor1
    }
  }

  "<=> and <~> have the same precedence" should {
    "be " + equivxor1 + " for " + F.equivxor1 in {
      F.equivxor1.fol must be equalTo equivxor1
    }
    "be " + xorequiv1 + " for " + F.xorequiv1 in {
      F.xorequiv1.fol must be equalTo xorequiv1
    }
  }

  "Right-associativity of =>, <=> and <~>" should {
    "be " + impl2 + " for " + F.impl2 in {
      F.impl2.fol must be equalTo impl2
    }
    "be " + equiv2 + " for " + F.equiv2 in {
      F.equiv2.fol must be equalTo equiv2
    }
    "be " + xor2 + " for " + F.xor2 in {
      F.xor2.fol must be equalTo xor2
    }
  }

  "Overriding precedences with ()" should {
    "be " + xoyz_br + " for " + F.xoyz_br in {
      F.xoyz_br.fol must be equalTo xoyz_br
    }
    "be " + xyoz_br + " for " + F.xyoz_br in {
      F.xyoz_br.fol must be equalTo xyoz_br
    }
    "be " + impl1_br + " for " + F.impl1_br in {
      F.impl1_br.fol must be equalTo impl1_br
    }
    "be " + equiv1_br + " for " + F.equiv1_br in {
      F.equiv1_br.fol must be equalTo equiv1_br
    }
    "be " + xor1_br + " for " + F.xor1_br in {
      F.xor1_br.fol must be equalTo xor1_br
    }
    "be " + xorequiv1_br + " for " + F.xorequiv1_br in {
      F.xorequiv1_br.fol must be equalTo xorequiv1_br
    }
    "be " + equivxor1_br + " for " + F.equivxor1_br in {
      F.equivxor1_br.fol must be equalTo equivxor1_br
    }
  }

  F.px should {
    "be parsed to " + px in {
      F.px.fol must be equalTo px
    }
  }

  F.pxy should {
    "be parsed to " + pxy in {
      F.pxy.fol must be equalTo pxy
    }
  }

  F.pxyz should {
    "be parsed to " + pxyz in {
      F.pxyz.fol must be equalTo pxyz
    }
  }

  F.pxyzw should {
    "be parsed to " + pxyzw in {
      F.pxyzw.fol must be equalTo pxyzw
    }
  }

  F.pab should {
    "be parsed to " + pab in {
      F.pab.fol must be equalTo pab
    }
  }

  F.pfp should {
    "be parsed to " + pfp in {
      F.pfp.fol must be equalTo pfp
    }
  }

  F.pfpgq should {
    "be parsed to " + pfpgq in {
      F.pfpgq.fol must be equalTo pfpgq
    }
  }

  F.pfpgqab should {
    "be parsed to " + pfpgqab in {
      F.pfpgqab.fol must be equalTo pfpgqab
    }
  }

  F.pfpagqb should {
    "be parsed to " + pfpagqb in {
      F.pfpagqb.fol must be equalTo pfpagqb
    }
  }

  F.axp should {
    "be parsed to " + axp in {
      F.axp.fol must be equalTo axp
    }
  }

  F.axyzp should {
    "be parsed to " + axyzp in {
      F.axyzp.fol must be equalTo axyzp
    }
  }

  F.exp should {
    "be parsed to " + exp in {
      F.exp.fol must be equalTo exp
    }
  }

  F.exyzp should {
    "be parsed to " + exyzp in {
      F.exyzp.fol must be equalTo exyzp
    }
  }

  F.h144 should {
    "be parsed to " + h144 in {
      F.h144.fol must be equalTo h144
    }
  }

  F.h150_1 should {
    "be parsed to " + h150_1 in {
      F.h150_1.fol must be equalTo h150_1
    }
  }

  F.h150_2 should {
    "be parsed to " + h150_2 in {
      F.h150_2.fol must be equalTo h150_2
    }
  }
}
