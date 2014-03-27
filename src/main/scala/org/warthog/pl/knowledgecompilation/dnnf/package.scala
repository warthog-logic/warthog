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

package org.warthog.pl.knowledgecompilation

import org.warthog.pl.io.CNFUtil
import org.warthog.pl.formulas.{ PLAtom, PL }
import collection.immutable.HashMap
import dnnf.simpleCompiler.dtree.{ Generator => SimpleDTreeGenerator }
import dnnf.advancedCompiler.dtree.{ SimpleComposeGenerator => AdvancedDTreeGenerator }
import dnnf.advancedCompiler.dtree.{ C2DDTreeGenerator => AdvancedC2DDTreeGenerator }
import dnnf.simpleCompiler.SimpleDNNFCompiler
import dnnf.advancedCompiler.AdvancedDNNFCompiler
import org.warthog.generic.parsers.DIMACSReader
import org.warthog.generic.formulas.{ Not, Formula }
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.core.MSJCoreProver

/**
  * Contains various methods for compiling a formula or file into a DNNF
  * The compilation may either be performed by the simple or advanced compiler
  * For example there are methods for compiling:
  *  - a Formula[PL] into DNNF
  *  - a List[Set[Lit] into DNNF
  *  - a Formula[PL] into a Formula[PL] which is a DNNF
  *  - a dimacs-file into DNNF
  *  - a dimacs-file into DNNF using a specific dtree produced by c2d-compiler
  */
package object dnnf {

  /**
    * Print information about the compilation process (recursive calls and cache hits)
    */
  val verbose = false

  sealed trait CompilerVersion
  case object Simple extends CompilerVersion
  case object Advanced extends CompilerVersion

  /**
    * Compiles a Formula[PL] into a DNNF
    * Example usage: Compile the formula "a & b | c" into DNNF using the advanced compiler:
    *    compile(Advanced, "a & b | c".pl)
    * @param version The compiler to use (Simple or Advanced)
    * @param formula The formula to compile
    * @return The corresponding d-DNNF
    */
  def compile(version: CompilerVersion, formula: Formula[PL]): DNNF = {
    val cnf = if (formula.isCNF) formula else formula.cnf
    val origClauses = CNFUtil.toList(cnf).map(_.toSet[Formula[PL]])
    var mapping = HashMap[String, Int]()
    var nextVar: Int = 0
    val clauses = origClauses.map(_.map {
      case PLAtom(name) => mapping.get(name) match {
        case Some(lit) => Lit(lit, true)
        case None      => mapping += (name -> nextVar); nextVar += 1; Lit(nextVar - 1, true)
      }
      case Not(PLAtom(name)) => mapping.get(name) match {
        case Some(lit) => Lit(lit, false)
        case None      => mapping += (name -> nextVar); nextVar += 1; Lit(nextVar - 1, false)
      }
      case _ => throw new Exception("This should not happen!!")
    })
    val dnnf = compile(version, clauses)

    DNNF.simplify(dnnf.substituteLits(mapping.map(_.swap)))
  }

  /** Compiles a List[Set[Int]] into a corresponding DNNF */
  def compile(version: CompilerVersion, clauses: List[Set[Lit]]): DNNF =
    if (clauses.isEmpty)
      True
    else
      version match {
        case Simple => {
          val dtree = SimpleDTreeGenerator.generateDTree(clauses)
          val compiler = new SimpleDNNFCompiler(clauses.size, dtree.varSet.size)
          val result = DNNF.simplify(compiler.cnf2Ddnnf(dtree))
          if (verbose) { println("---\nRecursive Calls: " + compiler.recursiveCalls + "\nCache Hits: " + compiler.cacheHits) }
          result
        }
        case Advanced => {
          val solverClauses = clauses map (_.map(v => MSJCoreProver.mkLit(v.variable, !v.phase)))
          val dtree = AdvancedDTreeGenerator.generateDTree(solverClauses)
          val compiler = new AdvancedDNNFCompiler(clauses.size, dtree.varSet.size)

          if (!compiler.initSolver(solverClauses))
            False
          else {
            val result = DNNF.simplify(compiler.cnf2dnnf(dtree))
            if (verbose) { println("---\nRecursive Calls: " + compiler.recursiveCalls + "\nCache Hits: " + compiler.cacheHits) }
            result
          }

        }
      }

  /** Compiles a Formula[PL] into a corresponding Formula[PL] which is a DNNF */
  def compileToPL(version: CompilerVersion, formula: Formula[PL]) = compile(version, formula).asPLFormula

  /** Compiles a dimacs-file into a corresponding DNNF */
  //This would be nicer but does not work for whatever reasons
  //def compileDIMACS(version: CompilerVersion, file: String) = compile(version, DIMACSReader.dimacs2Clauses(file).map(_.map(l => Lit(math.abs(l), l > 0))))
  def compileDIMACS(version: CompilerVersion, file: String) = compile(version, DIMACSReader.dimacs2Formula(file))

  /**
    * Compiles a cnf from a dimacs-file into a corresponding DNNF
    * using the c2d-compiler to get the dtree.
    *
    * Currently this method will be orders of magnitude faster than
    * the other methods which use a very primitive dtree.
    *
    * Important note: This method will not be deterministic,
    * since some actions in dtree-generation are randomized.
    * So this method might create different dnnfs with each call!
    * @param dimacsFile The path to the dimacs-file
    * @return The corresponding DNNF
    */
  def compileWithC2DDTree(dimacsFile: String) = {
    val clauses = DIMACSReader.dimacs2Clauses(dimacsFile)
    val solverClauses = clauses map (_.map(v => MSJCoreProver.mkLit(math.abs(v), v < 0)))
    val dtree = AdvancedC2DDTreeGenerator.generateDTree(dimacsFile)

    val compiler = new AdvancedDNNFCompiler(clauses.size, dtree.varSet.size)
    if (!compiler.initSolver(solverClauses))
      False
    else {
      val result = DNNF.simplify(compiler.cnf2dnnf(dtree))
      if (verbose) { println("---\nRecursive Calls: " + compiler.recursiveCalls + "\nCache Hits: " + compiler.cacheHits) }
      result
    }
  }

  /* ---------------------------------
   * Temporary test methods
   * --------------------------------- */

  /* Prints an evaluation of the compilation of the specified cnf to stdout
   *   version: 0 = all, 1 = Simple Compiler, 2 = Advanced Compiler, 3 = Advanced Compiler with C2DDTree
   */
  def evaluation(version: Int, dimacsFile: String, forceCounting: Boolean = false): Unit = version match {
    case 0 =>
      println("\n--- Simple ---"); evaluation(1, dimacsFile);
      println("\n--- Advanced ---"); evaluation(2, dimacsFile);
      println("\n--- AdvancedWithC2DDTree ---"); evaluation(3, dimacsFile);
    case _ =>
      val start = System.currentTimeMillis()
      val dnnf = version match {
        case 1 => compileDIMACS(Simple, dimacsFile)
        case 2 => compileDIMACS(Advanced, dimacsFile)
        case 3 => compileWithC2DDTree(dimacsFile)
        case _ => throw new Exception("Wrong call of method \"evaluation\": 0 = all, 1 = Simple Compiler, 2 = Advanced Compiler, 3 = Advanced Compiler with C2DDTree")
      }
      val end = System.currentTimeMillis()

      println("Time: " + (end - start) + "ms")
      println("Nodes: " + DNNF.nodeCount(dnnf))
      println("Nodes2: " + DNNF.nodeCount2(dnnf))
      if (DNNF.nodeCount2(dnnf) < 1000000 || forceCounting) println("Models: " + DNNF.countModels(dnnf, DIMACSReader.numberOfVariablesAndClauses(dimacsFile)._1))
  }

  def getSimpleDTree(formula: Formula[PL]) = {
    val cnf = if (formula.isCNF) formula else formula.cnf
    val origClauses = CNFUtil.toList(cnf).map(_.toSet[Formula[PL]])
    var mapping = HashMap[String, Int]()

    var nextLit: Int = 1
    val clauses = origClauses.map(_.map(atom => atom match {
      case PLAtom(name) => mapping.get(name) match {
        case Some(lit) => Lit(lit, true)
        case None      => mapping += (name -> nextLit); nextLit += 1; Lit(nextLit - 1, true)
      }
      case Not(PLAtom(name)) => mapping.get(name) match {
        case Some(lit) => Lit(lit, false)
        case None      => mapping += (name -> nextLit); nextLit += 1; Lit(nextLit - 1, false)
      }
      case _ => throw new Exception("This should not happen!!")
    }))

    SimpleDTreeGenerator.generateDTree(clauses)
  }

  def getAdvancedDTree(formula: Formula[PL]) = {
    val cnf = if (formula.isCNF) formula else formula.cnf
    val origClauses = CNFUtil.toList(cnf).map(_.toSet[Formula[PL]])
    var mapping = HashMap[String, Int]()

    var nextLit: Int = 1
    val clauses = origClauses.map(_.map(atom => atom match {
      case PLAtom(name) => mapping.get(name) match {
        case Some(lit) => Lit(lit, true)
        case None      => mapping += (name -> nextLit); nextLit += 1; Lit(nextLit - 1, true)
      }
      case Not(PLAtom(name)) => mapping.get(name) match {
        case Some(lit) => Lit(lit, false)
        case None      => mapping += (name -> nextLit); nextLit += 1; Lit(nextLit - 1, false)
      }
      case _ => throw new Exception("This should not happen!!")
    }))

    val solverClauses = clauses map (_.map(v => MSJCoreProver.mkLit(v.variable, !v.phase)))
    AdvancedDTreeGenerator.generateDTree(solverClauses)
  }
}
