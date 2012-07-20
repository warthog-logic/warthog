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

package fol

/**
  * A collection of interesting test generic in first order logic
  */

object F {
  /**
    * simple generic to test connectives
    */
  // atoms
  val verum = "$true"
  val falsum = "$false"
  val x = "x"
  val y = "y"
  val notx = "~x"
  val noty = "~y"

  // binary operators
  val x_impl_y = "x => y"
  val y_impl_x = "x <= y"
  val x_equiv_y = "x <=> y"
  val x_xor_y = "x <~> y"

  // nary operators
  val xy = "x & y"
  val xyz = "x & y & z"
  val xoy = "x | y"
  val xoyoz = "x | y | z"
  val xynz = "x & y & ~z"
  val nxoyoz = "~x | y | z"
  val n_xynz = "~(x & y & ~z)"
  val n_nxoyoz = "~(~x | y | z)"

  // combinations
  val xoyz = "x | y & z"
  val xyoz = "x & y | z"
  val impl1 = "x | ~y => ~z & y"
  val impl2 = "x => ~y => ~z"
  val impl1vv = "x | ~y <= ~z & y"
  val equiv1 = "x => ~y <=> ~z <= y"
  val equiv2 = "x <=> ~y <=> ~z"
  val xor1 = "x => ~y <~> ~z <= y"
  val xor2 = "x <~> ~y <~> ~z"
  val xorequiv1 = "x <~> ~y <=> ~z"
  val equivxor1 = "x <=> ~y <~> ~z"

  // expressions with brackets
  val xoyz_br = "(x | y) & z"
  val xyoz_br = "x & (y | z)"
  val impl1_br = "(x | (~y => ~z)) & y"
  val equiv1_br = "(x => (~y <=> ~z)) <= y"
  val xor1_br = "(x => (~y <~> ~z)) <= y"
  val xorequiv1_br = "(x <~> ~y) <=> ~z"
  val equivxor1_br = "(x <=> ~y) <~> ~z"

  // simple predicates
  val px = "p(X)"
  val pxy = "p(X,Y)"
  val pxyz = "p(X,Y,Z)"
  val pxyzw = "p(X,Y,Z,W)"

  // predicates + functions
  val pab = "p(a,b)"
  val pfp = "p(f(P))"
  val pfpgq = "p(f(P),g(Q))"
  val pfpgqab = "p(f(P),g(Q),a,b)"
  val pfpagqb = "p(f(P,a),g(Q,b))"

  // quantifiers
  val axp = "![X]: p"
  val axyzp = "![X,Y,Z]: p"
  val exp = "?[X]: p"
  val exyzp = "?[X,Y,Z]: p"

  // examples from Harrisons book
  val h144 = "(![X]: p(X) | r(Y)) => ?[Y,Z]: q(Y) | ~?[Z]: p(Z) & q(Z)"
  val h150_1 = "?[Y]: X < Y => ![U]: ?[V]: mul(X,U) < mul(Y,V)"
  val h150_2 = "![X]: p(X) => (?[X,Y]: q(Y)) | ~?[Z]: p(Z) & q(Z)"
}
