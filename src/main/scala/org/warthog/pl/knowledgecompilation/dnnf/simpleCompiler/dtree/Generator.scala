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

package org.warthog.pl.knowledgecompilation.dnnf.simpleCompiler.dtree

import org.warthog.pl.knowledgecompilation.dnnf._

/**
 * Contains methods for generating DTrees (simple compiler)
 */
object Generator {

  /**
   * Generates a DTree using a primitive algorithm described in
   * A. Darwiche "Decomposable Negation Normal Form"
   *
   * @param clauses The list of clauses
   * @return A dtree containing the clauses in its leafs
   */
  def generateDTree(clauses: List[Set[Lit]], reverseCompose: Boolean = true): DTree = {
    val pi = clauses.toSet.flatten.map(_.variable)
    var sigma: List[DTree] = Nil
    var currentID = 0
    clauses.foreach(c => {
      sigma ::= DTreeLeaf(currentID, c)
      currentID += 1
    })

    pi.foreach(v => {
      val gamma: List[DTree] = sigma.filter(_.varSet.contains(v))
      sigma = compose(gamma, reverseCompose) ++ sigma.filterNot(gamma.contains(_))
    })

    return compose(sigma, reverseCompose).head
  }

  private def compose(trees: List[DTree], reverse: Boolean): List[DTree] = trees.headOption match {
    case None => List()
    case Some(_) => List(if (reverse) reverseCompose(trees) else compose(trees))
  }

  private def compose(trees: List[DTree]): DTree =
    trees.tail.foldRight(trees.head)(DTreeNode(_, _))

  /* This method seems to produce slightly better dtrees for the compilation process */
  private def reverseCompose(trees: List[DTree]): DTree = trees.size match {
    case 1 => trees.head
    case _ =>
      val (a, b) = trees.splitAt(trees.size / 2)
      DTreeNode(reverseCompose(a), reverseCompose(b))
  }
}
