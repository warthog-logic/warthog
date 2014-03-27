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

package org.warthog.pl.knowledgecompilation.dnnf

/**
  * Representation of a Bit Vector
  */
class BitVec(val arr: Array[Long]) {
  private val capacity = arr.length * BitVec.LONGSIZE

  def this(capacity: Int) = this(new Array[Long](BitVec.requiredSize(capacity)))

  def this() = this(1024)

  override def hashCode = java.util.Arrays.hashCode(arr)

  private def ensure(index: Int) =
    if (index >= capacity)
      new BitVec(java.util.Arrays.copyOf(arr, BitVec.requiredSize(index)))
    else
      this

  def set(index: Int) = {
    val rv = ensure(index)
    rv.arr(index / BitVec.LONGSIZE) = rv.arr(index / BitVec.LONGSIZE) | (1L << (index % BitVec.LONGSIZE))
    rv
  }

  def unset(index: Int) = {
    val rv = ensure(index)
    rv.arr(index / BitVec.LONGSIZE) = rv.arr(index / BitVec.LONGSIZE) & ~(1L << (index % BitVec.LONGSIZE))
    rv
  }

  override def toString = arr.map(_.toBinaryString).mkString("\n")

  override def equals(that: Any) = that match {
    case bv: BitVec =>
      if (arr.length == bv.arr.length)
        (0 until arr.length).forall {
          i => arr(i) == bv.arr(i)
        }
      else
        false

    case _ => false
  }
}

object BitVec {
  val LONGSIZE = 64

  def requiredSize(index: Int) = index / LONGSIZE + 1
}