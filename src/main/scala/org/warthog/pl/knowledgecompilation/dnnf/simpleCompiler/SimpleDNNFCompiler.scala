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

package org.warthog.pl.knowledgecompilation.dnnf.simpleCompiler

import dtree._
import org.warthog.pl.knowledgecompilation.dnnf._
import org.warthog.pl.knowledgecompilation.dnnf.DNNF._
import collection.mutable.{ WeakHashMap, HashMap => MutableHashMap }
import org.warthog.pl.knowledgecompilation.dnnf.datastructures.BitVec

/**
  * A Simple Compiler for d-DNNF (deterministic DNNF) according to the algorithm of
  * Adnan Darwiche: "A compiler for deterministic, decomposable negation normal",
  * but with an improved caching implementation as supposed in
  * "New Advances in CNF to Decomposable Negation Normal Form"
  *
  * @param numClauses The number of clauses contained in the dtree
  * @param numVariables The number of variables contained in the dtree
  */
class SimpleDNNFCompiler(numClauses: Int, numVariables: Int) {

  /* Fields for unique nodes and caching */
  private val andUnique = MutableHashMap[Set[DNNF], DNNF]()
  private val orUnique = MutableHashMap[Set[DNNF], DNNF]()
  private val cache = WeakHashMap[BitVec, DNNF]()

  /* for debugging */
  var cachingEnabled = true

  /**
    * Compiles a dtree into a corresponding d-DNNF
    * Implementation according to Adnan Darwiche: "A compiler for deterministic, decomposable negation normal"
    * @param dtree The dtree to compile
    * @param omega The set of literals which are already set
    * @return The resulting d-DNNF
    */
  def cnf2Ddnnf(dtree: DTree, omega: Set[Lit] = Set()): DNNF = {
    trackRecursiveCall
    dtree match {
      case DTreeLeaf(_, clause) => clause2dDNNF(clause)
      case _ =>
        if (!cachingEnabled) return caseAnalysis(dtree, omega)

        val key = computeCacheKey(dtree.currentClauseIDs, dtree.varSet, omega)
        cache.get(key) match {
          case Some(v) =>
            trackHit
            v
          case None =>
            val result = caseAnalysis(dtree, omega)
            cache.put(key, result)
            result
        }
    }
  }

  /**
    * Converts clause {L_1,... , L_m}
    * into Or_i=1^m (L_i & AND_j=1^(i-1)(-L_j))
    * @param clause The Clause to convert
    * @return The Clause in dDNNF
    */
  private def clause2dDNNF(clause: Set[Lit]): DNNF = {
    val lits = clause.toList
    val ands = lits.foldLeft[List[List[DNNF]]](List(List()))((as, lit) => as ++ List(as.takeRight(1).head ++ List(neg(lit))))
    disjoin(lits.zip(ands).map(t => conjoin((List(t._1) ++ t._2): _*)): _*)
  }

  private def caseAnalysis(node: DTree, omega: Set[Lit]): DNNF = node match {
    case DTreeLeaf(_, l) => clause2dDNNF(l)
    case node: DTreeNode =>
      val sigma = node.currentSeparator.toList
      if (sigma.isEmpty) {
        conjoin(cnf2Ddnnf(node.lChild, omega), cnf2Ddnnf(node.rChild, omega))
      } else {
        val occ = node.countOccurrences(sigma)
        val X = sigma(occ.indexWhere(_ == occ.max))

        val X_plus = Lit(X, true)
        val X_minus = neg(X_plus)
        /* positive case */
        val (newNode0, pi0) = whileCase(node, X_plus, Set(X_plus))
        val alpha_plus =
          if (pi0.isEmpty)
            False
          else if (newNode0.isInstanceOf[DTreeLeaf] && newNode0.asInstanceOf[DTreeLeaf].clause.isEmpty)
            conjoin(pi0, True)
          else
            conjoin(pi0, caseAnalysis(newNode0, omega union pi0))
        /* negative case */
        val (newNode1, pi1) = whileCase(node, X_minus, Set(X_minus))
        val alpha_minus = if (pi1.isEmpty)
          False
        else if (newNode1.isInstanceOf[DTreeLeaf] && newNode1.asInstanceOf[DTreeLeaf].clause.isEmpty)
          conjoin(pi1, True)
        else
          conjoin(pi1, caseAnalysis(newNode1, omega union pi1))

        disjoin(alpha_plus, alpha_minus)
      }
  }

  private def whileCase(node: DTree, lit: Lit, pi: Set[Lit]): (DTree, Set[Lit]) = {

    /**
      * Substitutes lit in tree by True and neg(lit) by False
      * Returns None if one or more leafs evaluate to False
      * @param tree The dtree to substitute
      * @param lit The literal which is to be set
      * @return The substituted dtree or None if one or more clauses are empty
      */
    def substitute(tree: DTree, lit: Lit): Option[DTree] = tree match {
      case DTreeLeaf(id, clause) =>
        if (clause.contains(lit))
          Some(DTreeLeaf(id, Set())) /* clause satisfied */
        else if (clause.contains(neg(lit)))
          if (clause.size == 1) None /* clause not satisfied */ else Some(DTreeLeaf(id, clause - neg(lit)))
        else Some(tree)
      case DTreeNode(l, r) =>
        val left = substitute(l, lit)
        val right = substitute(r, lit)
        (left, right) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(a), Some(b)) => Some(
            if (a.isInstanceOf[DTreeLeaf] && a.asInstanceOf[DTreeLeaf].clause.isEmpty)
              b
            else if (b.isInstanceOf[DTreeLeaf] && b.asInstanceOf[DTreeLeaf].clause.isEmpty)
              a
            else
              DTreeNode(a, b))
        }
    }

    def findUnits(tree: DTree): Set[Lit] = tree match {
      case DTreeNode(a, b)  => findUnits(a) union findUnits(b)
      case DTreeLeaf(id, c) => if (c.size == 1) c else Set()
    }

    var units = Set(lit)
    var newTree = node
    var newPi = pi
    while (!units.isEmpty) {
      val unit = units.head
      units -= unit
      substitute(newTree, unit) match {
        case None    => return (DTreeLeaf(-1, Set()), Set()) /* contradiction found */
        case Some(t) => newTree = t
      }

      val newUnits = findUnits(newTree)
      units ++= newUnits
      newPi ++= newUnits
      if (!(newPi intersect newPi.map(neg)).isEmpty) return (DTreeLeaf(-1, Set()), Set()) /* contradiction found */
    }
    return (newTree, newPi)
  }

  /**
    * Compute a cache key as described in "New Advances in CNF to Decomposable
    * Negation Normal Form": Key is made up of a bit vector in which for each
    * unsubsumed clause and each instantiated variable a binary flag is set.
    *
    * @param clauseIds unsubsumed clauses
    * @param varSet variable set to scan for instantiated entries
    * @return generated cache key
    */
  private def computeCacheKey(clauseIds: Set[Int], varSet: Set[Int], omega: Set[Lit]) = {
    val key = new BitVec(numClauses + numVariables)
    val setVars = omega.map(_.variable)

    for (cls <- clauseIds)
      key.set(cls)
    for (v <- varSet if !(setVars contains v))
      key.set(v + numClauses)

    key
  }

  /* Some methods for conjoining/ disjoining DNNFs */
  private def conjoin(a: Set[Lit], b: DNNF): DNNF = conjoin(a.toList ++ List(b): _*)
  private def conjoin(args: DNNF*): DNNF = andUnique.getOrElseUpdate(args.toSet, And(args))
  private def disjoin(args: DNNF*): DNNF = orUnique.getOrElseUpdate(args.toSet, Or(args))

  /* Tracking cache hits and recursive calls */
  var cacheHits = 0L
  var recursiveCalls = 0L
  private def trackHit = {
    cacheHits += 1
    if (verbose && (cacheHits < 100000 && cacheHits % 1000 == 0 || cacheHits % 10000 == 0))
      println("tracked cache hits: " + cacheHits)
  }
  private def trackRecursiveCall = {
    recursiveCalls += 1
    if (verbose && (recursiveCalls < 100000 && recursiveCalls % 1000 == 0 || recursiveCalls % 10000 == 0))
      println("recursive calls of cnf2Ddnnf: " + recursiveCalls)
  }
}