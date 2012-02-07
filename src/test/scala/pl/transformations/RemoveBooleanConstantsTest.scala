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

package pl.transformations

import org.specs2.mutable._
import org.warthog.generic.formulas._
import org.warthog.pl.parsers._
import org.warthog.pl.formulas.PLAtom

/**
 * Tests for Boolean constant removal
 *
 * Author: zengler
 * Date:   25.01.12
 */
class RemoveBooleanConstantsTest extends Specification {
  val (x, y, z) = (PLAtom("x"), PLAtom("y"), PLAtom("z"))

  val verum = Verum()
  val falsum = Falsum()

  val notT = -verum
  val notF = -falsum
  val impFX = Implication(falsum, x)
  val impXT = Implication(x, verum)
  val impTX = Implication(verum, x)
  val impXF = Implication(x, falsum)
  val equivTX = Equiv(verum, x)
  val equivXT = Equiv(x, verum)
  val equivFX = Equiv(falsum, x)
  val equivXF = Equiv(x, falsum)
  val xorTX = Xor(verum, x)
  val xorXT = Xor(x, verum)
  val xorFX = Xor(falsum, x)
  val xorXF = Xor(x, falsum)
  val andF = And(x, verum, falsum, y, z)
  val andT = And(verum, verum, verum)
  val and = And(verum, x, verum, y, verum)
  val orT = Or(x, verum, falsum, y, z)
  val orF = Or(falsum, falsum, falsum)
  val or = Or(falsum, x, falsum, y, falsum)

  val harrison1 = Implication(Implication(verum, Equiv(x, falsum)), -y || falsum && z)
  val harrison2 = Implication(Implication(x, y), verum) || -falsum

  "Constant Elimination results in" should {
    "~$true = $false" in {
      "~$true".pl.removeBooleanConstants must be equalTo "$false".pl
    }
    "~$false = $true" in {
      "~$false".pl.removeBooleanConstants must be equalTo "$true".pl
    }
    "$false => x = $true" in {
      "$false => x".pl.removeBooleanConstants must be equalTo "$true".pl
    }
    "$true => x = x" in {
      "$true => x".pl.removeBooleanConstants must be equalTo "x".pl
    }
    "x => $false = ~x" in {
      "x => $false".pl.removeBooleanConstants must be equalTo "~x".pl
    }
    "x => $true = x" in {
      "x => $true".pl.removeBooleanConstants must be equalTo "$true".pl
    }
    "$true <=> x = x" in {
      "$true <=> x".pl.removeBooleanConstants must be equalTo "x".pl
    }
    "x <=> $true = x" in {
      "x <=> $true".pl.removeBooleanConstants must be equalTo "x".pl
    }
    "$false <=> x = ~x" in {
      "$false <=> x".pl.removeBooleanConstants must be equalTo "~x".pl
    }
    "x <=> $false = ~x" in {
      "x <=> $false".pl.removeBooleanConstants must be equalTo "~x".pl
    }
    "$true <~> x = ~x" in {
      "$true <~> x".pl.removeBooleanConstants must be equalTo "~x".pl
    }
    "x <~> $true = ~x" in {
      "x <~> $true".pl.removeBooleanConstants must be equalTo "~x".pl
    }
    "$false <~> x = x" in {
      "$false <~> x".pl.removeBooleanConstants must be equalTo "x".pl
    }
    "x <~> $false = x" in {
      "x <~> $false".pl.removeBooleanConstants must be equalTo "x".pl
    }
    "x & $true & $false & y & z = $false" in {
      "x & $true & $false & y & z".pl.removeBooleanConstants must be equalTo "$false".pl
    }
    "$true & $true & $true = $true" in {
      "$true & $true & $true".pl.removeBooleanConstants must be equalTo "$true".pl
    }
    "$true & x & $true & y & $true = x & y" in {
      "$true & x & $true & y & $true".pl.removeBooleanConstants must be equalTo "x & y".pl
    }
    "x | $true | $false | y | z = $true" in {
      "x | $true | $false | y | z".pl.removeBooleanConstants must be equalTo "$true".pl
    }
    "$false | $false | $false = $false" in {
      "$false | $false | $false".pl.removeBooleanConstants must be equalTo "$false".pl
    }
    "$false | x | $false | y | $false = x | y" in {
      "$false | x | $false | y | $false".pl.removeBooleanConstants must be equalTo "x | y".pl
    }
    "($true => (x <=> $false)) => ~(y | $false & z) = ~x => ~y" in {
      "($true => (x <=> $false)) => ~(y | $false & z)".pl.removeBooleanConstants must be equalTo "~x => ~y".pl
    }
    "((x => y) => $true) | ~$false = $true" in {
      "((x => y) => $true) | ~$false".pl.removeBooleanConstants must be equalTo "$true".pl
    }
  }
}
