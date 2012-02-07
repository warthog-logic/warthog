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

package pl.formulas

import org.specs2.mutable._
import pl.F
import org.warthog.pl.formulas.PLAtom
import org.warthog.pl.parsers._

/**
 * Basic tests for propositional generic
 * + Flatteing
 * + NNF
 * + Ground
 * + Sets of bound/free variables
 *
 * Author: zengler
 * Date:   25.01.12
 */
class BasicFormulaTest extends Specification {
  val (x,y,z) = (PLAtom("x"), PLAtom("y"), PLAtom("z"))

  "$true" should {
    "should be flattened to T" in {
      F.verum.pl.booleanFlatten must be equalTo F.verum.pl
    }
    "be in NNF" in {
      F.verum.pl.isNNF must be equalTo true
    }
    "have NNF T" in {
      F.verum.pl.nnf must be equalTo F.verum.pl
    }
    "be ground" in {
      F.verum.pl.isGround must be equalTo true
    }
    "have an empty set of variables" in {
      F.verum.pl.vars must have size 0
    }
    "have an empty set of free variables" in {
      F.verum.pl.freeVars must have size 0
    }
    "have an empty set of bound variables" in {
      F.verum.pl.boundVars must have size 0
    }
  }

  "$false" should {
    "should be flattened to F" in {
      F.falsum.pl.booleanFlatten must be equalTo F.falsum.pl
    }
    "be in NNF" in {
      F.falsum.pl.isNNF must be equalTo true
    }
    "have NNF F" in {
      F.falsum.pl.nnf must be equalTo F.falsum.pl
    }
    "be ground" in {
      F.falsum.pl.isGround must be equalTo true
    }
    "have an empty set of variables" in {
      F.falsum.pl.vars must have size 0
    }
    "have an empty set of free variables" in {
      F.falsum.pl.freeVars must have size 0
    }
    "have an empty set of bound variables" in {
      F.falsum.pl.boundVars must have size 0
    }
  }

  "Variable x" should {
    "should be flattened to x" in {
      F.x.pl.booleanFlatten must be equalTo x
    }
    "be in NNF" in {
      F.x.pl.isNNF must be equalTo true
    }
    "have NNF x" in {
      F.x.pl.nnf must be equalTo x
    }
    "not be ground" in {
      F.x.pl.isGround must be equalTo false
    }
    "have a set of variables {x}" in {
      F.x.pl.vars must be equalTo List(x)
    }
    "have a set of free variables {x}" in {
      F.x.pl.freeVars must be equalTo List(x)
    }
    "have an empty set of bound variables" in {
      F.x.pl.boundVars must have size 0
    }
  }

  "~x" should {
    "should be flattened to ~x" in {
      F.notx.pl.booleanFlatten must be equalTo F.notx.pl
    }
    "be in NNF" in {
      F.notx.pl.isNNF must be equalTo true
    }
    "have NNF ~x" in {
      F.notx.pl.nnf must be equalTo F.notx.pl
    }
    "not be ground" in {
      F.notx.pl.isGround must be equalTo false
    }
    "have a set of variables {x}" in {
      F.notx.pl.vars must be equalTo List(x)
    }
    "have a set of free variables {x}" in {
      F.notx.pl.freeVars must be equalTo List(x)
    }
    "have an empty set of bound variables" in {
      F.notx.pl.boundVars must have size 0
    }
  }

  "x => y" should {
    "should be flattened to ~x | y" in {
      F.x_impl_y.pl.booleanFlatten must be equalTo "~x | y".pl
    }
    "not be in NNF" in {
      F.x_impl_y.pl.isNNF must be equalTo false
    }
    "have NNF ~x | y" in {
      F.x_impl_y.pl.nnf must be equalTo "~x | y".pl
    }
    "not be ground" in {
      F.x_impl_y.pl.isGround must be equalTo false
    }
    "have a set of variables {x,y}" in {
      F.x_impl_y.pl.vars must be equalTo List(x, y)
    }
    "have a set of free variables {x,y}" in {
      F.x_impl_y.pl.freeVars must be equalTo List(x, y)
    }
    "have an empty set of bound variables" in {
      F.x_impl_y.pl.boundVars must have size 0
    }
  }

  "x <~> y" should {
    val flatXor = "(x & ~y) | (~x & y)"
    "should be flattened to " + flatXor in {
      F.x_xor_y.pl.booleanFlatten must be equalTo flatXor.pl
    }
    "not be in NNF" in {
      F.x_xor_y.pl.isNNF must be equalTo false
    }
    "have NNF " + flatXor in {
      F.x_xor_y.pl.nnf must be equalTo flatXor.pl
    }
    "not be ground" in {
      F.x_xor_y.pl.isGround must be equalTo false
    }
    "have a set of variables {x,y}" in {
      F.x_xor_y.pl.vars must be equalTo List(x, y)
    }
    "have a set of free variables {x,y}" in {
      F.x_xor_y.pl.freeVars must be equalTo List(x, y)
    }
    "have an empty set of bound variables" in {
      F.x_xor_y.pl.boundVars must have size 0
    }
  }

  "x <=> y" should {
    val flatEquiv = "(x & y) | (~x & ~y)"
    "should be flattened to " + flatEquiv in {
      F.x_equiv_y.pl.booleanFlatten must be equalTo flatEquiv.pl
    }
    "not be in NNF" in {
      F.x_equiv_y.pl.isNNF must be equalTo false
    }
    "have NNF " + flatEquiv in {
      F.x_equiv_y.pl.nnf must be equalTo flatEquiv.pl
    }
    "not be ground" in {
      F.x_equiv_y.pl.isGround must be equalTo false
    }
    "have a set of variables {x,y}" in {
      F.x_equiv_y.pl.vars must be equalTo List(x, y)
    }
    "have a set of free variables {x,y}" in {
      F.x_equiv_y.pl.freeVars must be equalTo List(x, y)
    }
    "have an empty set of bound variables" in {
      F.x_equiv_y.pl.boundVars must have size 0
    }
  }

  "x & y & ~z" should {
    "should be flattened to " + F.xynz in {
      F.xynz.pl.booleanFlatten must be equalTo F.xynz.pl
    }
    "be in NNF" in {
      F.xynz.pl.isNNF must be equalTo true
    }
    "have NNF " + F.xynz in {
      F.xynz.pl.nnf must be equalTo F.xynz.pl
    }
    "not be ground" in {
      F.xynz.pl.isGround must be equalTo false
    }
    "have a set of variables {x,y,z}" in {
      F.xynz.pl.vars must be equalTo List(x, y, z)
    }
    "have a set of free variables {x,y,z}" in {
      F.xynz.pl.freeVars must be equalTo List(x, y, z)
    }
    "have an empty set of bound variables" in {
      F.xynz.pl.boundVars must have size 0
    }
  }

  "~x | y | z" should {
    "should be flattened to " + F.nxoyoz in {
      F.nxoyoz.pl.booleanFlatten must be equalTo F.nxoyoz.pl
    }
    "be in NNF" in {
      F.nxoyoz.pl.isNNF must be equalTo true
    }
    "have NNF " + F.nxoyoz in {
      F.nxoyoz.pl.nnf must be equalTo F.nxoyoz.pl
    }
    "not be ground" in {
      F.nxoyoz.pl.isGround must be equalTo false
    }
    "have a set of variables {x,y,z}" in {
      F.nxoyoz.pl.vars must be equalTo List(x, y, z)
    }
    "have a set of free variables {x,y,z}" in {
      F.nxoyoz.pl.freeVars must be equalTo List(x, y, z)
    }
    "have an empty set of bound variables" in {
      F.nxoyoz.pl.boundVars must have size 0
    }
  }

  "~(x & y & ~z)" should {
    val negandNF = "~x | ~y | z"
    "should be flattened to " + F.n_xynz in {
      F.n_xynz.pl.booleanFlatten must be equalTo F.n_xynz.pl
    }
    "not be in NNF" in {
      F.n_xynz.pl.isNNF must be equalTo false
    }
    "have NNF " + negandNF in {
      F.n_xynz.pl.nnf must be equalTo negandNF.pl
    }
    "not be ground" in {
      F.n_xynz.pl.isGround must be equalTo false
    }
    "have a set of variables {x,y,z}" in {
      F.n_xynz.pl.vars must be equalTo List(x, y, z)
    }
    "have a set of free variables {x,y,z}" in {
      F.n_xynz.pl.freeVars must be equalTo List(x, y, z)
    }
    "have an empty set of bound variables" in {
      F.n_xynz.pl.boundVars must have size 0
    }
  }

  "~(~x | y | z)" should {
    val negorNF = "x & ~y & ~z"
    "should be flattened to " + F.n_nxoyoz in {
      F.n_nxoyoz.pl.booleanFlatten must be equalTo F.n_nxoyoz.pl
    }
    "not be in NNF" in {
      F.n_nxoyoz.pl.isNNF must be equalTo false
    }
    "have NNF x /\\ -y /\\ -z" in {
      F.n_nxoyoz.pl.nnf must be equalTo negorNF.pl
    }
    "not be ground" in {
      F.n_nxoyoz.pl.isGround must be equalTo false
    }
    "have a set of variables {x,y,z}" in {
      F.n_nxoyoz.pl.vars must be equalTo List(x, y, z)
    }
    "have a set of free variables {x,y,z}" in {
      F.n_nxoyoz.pl.freeVars must be equalTo List(x, y, z)
    }
    "have an empty set of bound variables" in {
      F.n_nxoyoz.pl.boundVars must have size 0
    }
  }
}
