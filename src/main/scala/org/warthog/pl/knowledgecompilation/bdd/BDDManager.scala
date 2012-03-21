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

package org.warthog.pl.knowledgecompilation.bdd

import java.io.PrintStream
import org.warthog.pl.formulas.{PL, PLAtom}
import org.warthog.generic.formulas._
import scala.collection.mutable.{ HashMap => MutableHashMap }

/**
 * BDD Manager for ROBDDs with complemented edges
 * (c.f. Harrison: Handbook of Practical Logic and Automated Reasoning)
 *
 * Author: zengler
 * Date:   25.01.12
 */
class BDDManager(var ord: Seq[PLAtom] = Seq[PLAtom]()) {

  /**
   * Inner class representing a single BDD node
   *
   * Corresponds to Harrison's BDD((unique,uback,index),ord)
   * @param v the variable at this node
   * @param left the index of the left child
   * @param right the index of the right child
   */
  private case class BDDNode(v: PLAtom, left: Int, right: Int) {
    override def toString = "[%s | left: %s , right: %s]".format(v, left, right)

    /**
     * Get the complement of this node (i.e. negated left and right children)
     * @return the complemented node
     */
    def complement: BDDNode = BDDNode(v, -left, -right)
  }

  private var unique = Map[Int, BDDNode]()
  private var uback = Map[BDDNode, Int]()
  private var computeTable = Map[Set[Int], Int]()
  private var index = 2
  private var root = 0

  val bddTrue = 1
  val bddFalse = -1

  override def toString = "<BDD with %d nodes>".format(index)

  /**Get a Formula representation of the BDD indexed by m
   * @param m the index
   * @return the Formula representation of the BDD rooted at m
   */
  def toFormula(m: Int): Formula[PL] =
    if (m == 1)
      Verum()
    else if (m < 0)
      Not(toFormula(-m))
    else {
      val BDDNode(v, l, r) = expandNode(m)
      v && toFormula(l) || -v && toFormula(r)
    }

  /**
   * Get the corresponding BDD node to an index
   * @param n the index
   * @return the corresponding node
   */
  private def expandNode(n: Int): BDDNode =
    if (n >= 0)
      unique.getOrElse(n, BDDNode(null, 1, 1))
    else
      unique.get(-n) match {
        case Some(b) => b.complement
        case None    => BDDNode(null, 1, 1)
      }

  /**
   * Add a new node to the BDD.
   * If the node is not already present, add a new one.  In both cases return the index of the new node.
   * @param node the new node to add
   * @return the index of the new node
   */
  private def lookupUnique(node: BDDNode): Int = uback.get(node) match {
    case Some(i) => i
    case None    => {
      unique += (index -> node);
      uback += (node -> index);
      index += 1;
      index - 1
    }
  }

  /**
   * Extract the order of a BDD.
   * @param v1 the first variable for comparison
   * @param v2 the second variable for comparison
   * @return `true` if v1 < v2 wrt. the current ordering, `false` else
   */
  private def order(v1: PLAtom, v2: PLAtom): Boolean = (v2 == null && v1 != null) || ord.indexOf(v1) < ord.indexOf(v2)

  /**
   * Make a new BDD node and return its index.
   * @param v the variable at this node
   * @param left the index of the left child
   * @param right the index of the right child
   * @return the index of the new node
   */
  def mkNode(v: PLAtom, left: Int, right: Int): Int = {
    if (!ord.contains(v))
      ord :+= v
    if (left == right)
      left
    else if (left >= 0)
      lookupUnique(BDDNode(v, left, right))
    else
      -lookupUnique(BDDNode(v, -left, -right))
  }

  /**
   * Compute the conjunction of two BDDs.
   * @param m1 the first BDD (index of root node)
   * @param m2 the second BDD (index of root node)
   * @return m1 & m2 (index of root node)
   */
  def bddAnd(m1: Int, m2: Int): Int = {
    if (m1 == -1 || m2 == -1) -1
    else if (m1 == 1) m2
    else if (m2 == 1) m1
    else computeTable.getOrElse(Set(m1, m2), {
      val node1 = expandNode(m1)
      val node2 = expandNode(m2)
      val (p, lnew, rnew) = if (node1.v == node2.v)
        (node1.v, bddAnd(node1.left, node2.left), bddAnd(node1.right, node2.right))
      else if (order(node1.v, node2.v))
        (node1.v, bddAnd(node1.left, m2), bddAnd(node1.right, m2))
      else
        (node2.v, bddAnd(m1, node2.left), bddAnd(m1, node2.right))
      val result = mkNode(p, lnew, rnew)
      computeTable += (Set(m1, m2) -> result)
      result
    }
    )
  }

  /**
   * Compute the disjunction of two BDDs.
   * @param m1 the first BDD (index of root node)
   * @param m2 the second BDD (index of root node)
   * @return m1 | m2 (index of root node)
   */
  def bddOr(m1: Int, m2: Int): Int = -bddAnd(-m1, -m2)

  /**
   * Compute the implication of two BDDs.
   * @param m1 the first BDD (index of root node)
   * @param m2 the second BDD (index of root node)
   * @return m1 => m2 (index of root node)
   */
  def bddImplication(m1: Int, m2: Int): Int = bddOr(-m1, m2)

  /**
   * Compute the equivalence of two BDDs.
   * @param m1 the first BDD (index of root node)
   * @param m2 the second BDD (index of root node)
   * @return m1 <=> m2 (index of root node)
   */
  def bddEquiv(m1: Int, m2: Int): Int = bddOr(bddAnd(m1, m2), bddAnd(-m1, -m2))

  /**
   * Compute the xor of two BDDs.
   * @param m1 the first BDD (index of root node)
   * @param m2 the second BDD (index of root node)
   * @return m1 XOR m2 (index of root node)
   */
  def bddXor(m1: Int, m2: Int): Int = bddOr(bddAnd(m1, -m2), bddAnd(-m1, m2))

  /*
   * BDD-based boolean QE. Yet to be tested.
   */
  private def bddQE(m1: Int, vs: Set[PLAtom], elim: (Int, Int) => Int): Int =
    if (m1 == 1 || m1 == -1)
      m1
    else {
      val BDDNode(v, l, r) = expandNode(m1)
      if (vs.contains(v))
        elim(bddQE(l, vs, elim), bddQE(r, vs, elim))
      else
        mkNode(v, bddQE(l, vs, elim), bddQE(r, vs, elim))
    }

  def bddExists(m: Int, vs: Set[PLAtom]) = bddQE(m, vs, bddOr)

  def bddForAll(m: Int, vs: Set[PLAtom]) = bddQE(m, vs, bddAnd)

  /**
   * Compute the BDD for a given formula.
   * @param f the formula
   * @return the corresponding BDD (index of root node)
   */
  def mkBDD(f: Formula[PL]): Int = f match {
    case a: Falsum[PL]     => bddFalse
    case a: Verum[PL]      => bddTrue
    case v@PLAtom(_)       => mkNode(v, 1, -1)
    case Not(p)            => -mkBDD(p)
    case Implication(p, q) => bddImplication(mkBDD(p), mkBDD(q))
    case Equiv(p, q)       => bddEquiv(mkBDD(p), mkBDD(q))
    case Xor(p, q)         => bddXor(mkBDD(p), mkBDD(q))
    case And(fs@_*)        => fs.foldLeft(bddTrue)((bdd, op) => bddAnd(bdd, mkBDD(op)))
    case Or(fs@_*)         => fs.foldLeft(bddFalse)((bdd, op) => bddOr(bdd, mkBDD(op)))
  }

  /**
   * Is BDD m a tautology.
   * @param m the BDD to test for the tautology test (index of root node)
   * @return 'true' if m is a tautology, 'false' else
   */
  def isTautology(m: Int) = m == bddTrue

  /**
   * Is BDD m a contradiction.
   * @param m BDD to test for the conbtradiction test (index of root node)
   * @return 'true' if m is a contradiction, 'false' else
   */
  def isContradiction(m: Int) = m == bddFalse

  def debugInfo: String = {
    val init = "Unique Table\n------------------\n"
    unique.foldLeft(init)((s, p) => s + "Node %d: %s\n".format(p._1, p._2))
  }

  /**A printer for the Graphviz dot graph drawing utility
   *
   * @param m root of the BDD to output
   * @param out the output stream to print to
   */
  def printDot(m: Int, out: PrintStream = System.out) {
    val edgeSeen = MutableHashMap[(Int, Int), Boolean]()
    def edge(a: Int, b: Int, comp: Boolean, left: Boolean) = {
      if (!edgeSeen.isDefinedAt((a, b))) {
        edgeSeen.update((a, b), true)
        val es = if (left) "solid" else "dotted"
        if (comp)
          "  %d -> %d [arrowtail=dot,arrowhead=normal,style=%s];".format(a, b, es)
        else
          "  %d -> %d [style=%s];".format(a, b, es)
      } else ""
    }

    /* assumption: a>=0 */
    def visitnode(a: Int): Unit = {
      if (a > 1) {
        val BDDNode(v, l, r) = expandNode(a)

        out.println("\n  %d [label=%s];".format(a, v.name))
        out.println(edge(a, l.abs.toInt, l < 0, true))
        out.println(edge(a, r.abs.toInt, r < 0, false))

        visitnode(l.abs.toInt)
        visitnode(r.abs.toInt)
      }
    }

    /* start of digraph */
    out.println(
      """
      digraph {
        0 [style=invisible];
        1 [shape=box];

      """
    )

    out.println(edge(0, m.abs.toInt, m < 0, true))
    visitnode(m.abs.toInt)

    /* end of digraph */
    out.println("}")
  }
}
