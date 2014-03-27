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

package org.warthog.pl.knowledgecompilation.dnnf.advancedCompiler.dtree

import org.warthog.pl.knowledgecompilation.dnnf.DNNFOperations
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.core.MSJCoreProver
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.datastructures.LBool

/**
 * Trait for a Decomposition Tree (DTree) for the AdvancedCompiler
 * This is either a DTreeNode or a DTreeLeaf
 */
sealed trait DTree {

  /** All variables of this DTree */
  val varSet: Set[Int]

  /**
   * Computes the current variable set of this dtree
   * The current variable set includes all variables which:
   * - are not yet assigned and
   * - occur in clauses that are currently not subsumed
   * @param operations The DNNFOperations which knows the current variable assignment
   * @return The current variable set of this dtree
   */
  def currentVarSet(operations: DNNFOperations): Set[Int]

  /**
   * The current separator of this dtree
   * @param operations The DNNFOperations which knows the current variable assignment
   * @return The current separator of this dtree
   */
  def currentSeparator(operations: DNNFOperations): Set[Int]

  /**
   * The ids of all non-subsumed clauses in this dtree
   * @param operations The DNNFOperations which knows the current variable assignment
   * @return The current clause ids
   */
  def currentClauseIds(operations: DNNFOperations): Set[Int]

  /**
   * Counts the number of unsubsumed occurrences for each variable in vars
   * @param operations The DNNFOperations which knows the current variable assignment
   * @param vars An array of variables whose occurrences should be counted
   * @return An array containing the number of unsubsumed occurrences for each variable in vars
   */
  def countUnsubsumedOccurrences(operations: DNNFOperations, vars: Array[Int]): Array[Int]
}

/**
 * A DTreeNode with two children
 * @param left The left child which is another dtree
 * @param right The right child which is another dtree
 */
case class DTreeNode(val left: DTree, val right: DTree) extends DTree {
  lazy val varSet = left.varSet union right.varSet

  override def toString() = "Node(" + left + "," + right + ")"

  def currentVarSet(operations: DNNFOperations) = left.currentVarSet(operations) union right.currentVarSet(operations)

  /** The separator of a node is defined as the intersection of the variables sets of its children */
  def currentSeparator(operations: DNNFOperations) = left.currentVarSet(operations) intersect right.currentVarSet(operations)

  def currentClauseIds(operations: DNNFOperations) = left.currentClauseIds(operations) union right.currentClauseIds(operations)

  def countUnsubsumedOccurrences(operations: DNNFOperations, vars: Array[Int]) = {
    val l = left.countUnsubsumedOccurrences(operations, vars)
    val r = right.countUnsubsumedOccurrences(operations, vars)
    l zip r map (n => n._1 + n._2)
  }
}

/**
 * A DTreeLeaf represents a clause
 * @param clauseId An Int which uniquely identifies the clause
 * @param clause The clause represented by this DTreeLeaf
 */
case class DTreeLeaf(val clauseId: Int, val clause: Set[Int]) extends DTree {
  lazy val varSet = clause.map(MSJCoreProver.`var`(_))

  override def toString() = "Leaf(" + clauseId + ",{" + clause.map(l => (if (MSJCoreProver.sign(l)) "-" else "") + MSJCoreProver.`var`(l)).mkString(",") + "})"

  def clauseIsSubsumed(operations: DNNFOperations) = clause.exists(x => operations.valueOfLit(x) == LBool.TRUE)

  /*
   * compute current variable set of clause at dtree leaf,
   * \emptyset if clause is satisfied under the current assignment
   */
  def currentVarSet(operations: DNNFOperations): Set[Int] = {
    if (clauseIsSubsumed(operations))
      Set[Int]()
    else
      clause.filter(operations.valueOfLit(_) == LBool.UNDEF)
  }

  def currentClauseIds(operations: DNNFOperations) = {
    if (clauseIsSubsumed(operations))
      Set[Int]()
    else
      Set(clauseId)
  }

  /** The separator of a dtree leaf is trivially empty */
  def currentSeparator(solver: DNNFOperations) = Set.empty[Int]

  def countUnsubsumedOccurrences(operations: DNNFOperations, vars: Array[Int]) =
    if (clauseIsSubsumed(operations))
      Array.fill(vars.size)(0)
    else
      vars.map(v => if (varSet.contains(v)) 1 else 0)
}

object DTree {

  /**
   * Computes the cardinality of all separators in a dtree
   * @param dtree The dtree
   * @return The cardinality of all separators in the dtree
   */
  def separatorCardinality(dtree: DTree): Int = dtree match {
    case DTreeLeaf(_, clause) => 0
    case DTreeNode(left, right) => left.varSet.intersect(right.varSet).size + separatorCardinality(left) + separatorCardinality(right)
  }

  /**
   * Computes the cardinality of all separators in a dtree,
   * but the cardinality of each separator is multiplied with its depth in the dtree
   * @param dtree The dtree
   * @return The result
   */
  def separatorCardinalityWeighted(dtree: DTree): Double = {
    def sepCard(dtree: DTree, level: Double): Double = dtree match {
      case DTreeLeaf(_, clause) => 0L
      case DTreeNode(left, right) => level * left.varSet.intersect(right.varSet).size + sepCard(left, level * 9 / 10) + sepCard(right, level * 9 / 10)
    }
    sepCard(dtree, 100)
  }
}