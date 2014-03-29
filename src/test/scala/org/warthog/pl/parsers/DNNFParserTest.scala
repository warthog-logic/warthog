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

package org.warthog.pl.parsers

import org.warthog.pl.F
import org.specs2.mutable._
import org.warthog.pl.knowledgecompilation.dnnf._

/**
 * Test cases for the propositional logic parser
 */
class DNNFParserTest extends Specification {
  val verum = True
  val falsum = False
  val x = StringLit("x", true)
  val y = StringLit("y", true)
  val z = StringLit("z", true)
  val notx = StringLit("x", false)
  val noty = StringLit("y", false)
  val notz = StringLit("z", false)
  val xy = And(List(x, y))
  val xyz = And(List(x, y, z))
  val xoy = Or(List(x, y))
  val notxz = And(List(notx, z))
  val xnotz = And(List(x, notz))
  val notxnotz = And(List(notx, notz))
  val notxoz = Or(List(notx, z))
  val xonotz = Or(List(x, notz))
  val notxonotz = Or(List(notx, notz))
  val xoyoz = Or(List(x, y, z))
  val xoyz = Or(List(x, And(List(y, z))))
  val xyoz = Or(List(And(List(x, y)), z))

  val xoyz_br = And(List(Or(List(x, y)), z))
  val xyoz_br = And(List(x, Or(List(y, z))))

  F.verum should {
    "be parsed to " + verum in {
      F.verum.dnnf structuralEquals verum
    }
  }

  F.falsum should {
    "be parsed to " + falsum in {
      F.falsum.dnnf structuralEquals falsum
    }
  }

  F.x should {
    "be parsed to " + x in {
      F.x.dnnf structuralEquals x
    }
  }

  F.y should {
    "be parsed to " + y in {
      F.y.dnnf structuralEquals y
    }
  }

  F.notx should {
    "be parsed to " + notx in {
      F.notx.dnnf structuralEquals notx
    }
  }

  F.noty should {
    "be parsed to " + noty in {
      F.noty.dnnf structuralEquals noty
    }
  }

  F.xy should {
    "be parsed to " + xy in {
      F.xy.dnnf structuralEquals xy
    }
  }

  F.xyz should {
    "be parsed to " + xyz in {
      F.xyz.dnnf structuralEquals xyz
    }
  }

  F.xoy should {
    "be parsed to " + xoy in {
      F.xoy.dnnf structuralEquals xoy
    }
  }

  F.xoyoz should {
    "be parsed to " + xoyoz in {
      F.xoyoz.dnnf structuralEquals xoyoz
    }
  }

  "Precedences of - and &" should {
    "be " + notxz + " for ~x & z" in {
      "~x & z".dnnf structuralEquals notxz
    }
    "be " + xnotz + " for x & ~z" in {
      "x & ~z".dnnf structuralEquals xnotz
    }
    "be " + notxnotz + " for ~x & ~z" in {
      "~x & ~z".dnnf structuralEquals notxnotz
    }
  }

  "Precedences of - and |" should {
    "be " + notxoz + " for ~x | z" in {
      "~x | z".dnnf structuralEquals notxoz
    }
    "be " + xonotz + " for x | ~z" in {
      "x | ~z".dnnf structuralEquals xonotz
    }
    "be " + notxonotz + " for ~x | ~z" in {
      "~x | ~z".dnnf structuralEquals notxonotz
    }
  }

  "Precedences of & and |" should {
    "be " + xoyz + " for x | y & z" in {
      F.xoyz.dnnf structuralEquals xoyz
    }
    "be " + xyoz + " for x & y | z" in {
      F.xyoz.dnnf structuralEquals xyoz
    }
  }

  "Overriding precedences with ()" should {
    "be " + xoyz_br + " for " + F.xoyz_br in {
      F.xoyz_br.dnnf structuralEquals xoyz_br
    }
    "be " + xyoz_br + " for " + F.xyoz_br in {
      F.xyoz_br.dnnf structuralEquals xyoz_br
    }
  }
}
