package org.warthog.pl.knowledgecompilation.dnnf

import org.warthog.pl.io.CNFUtil
import org.warthog.pl.formulas.{ PLAtom, PL }
import collection.immutable.HashMap
import org.warthog.pl.knowledgecompilation.dnnf.DNNFCompilation._
import org.warthog.pl.knowledgecompilation.dnnf.simpleCompiler.dtree.{ Generator => SimpleDTreeGenerator }
import org.warthog.pl.knowledgecompilation.dnnf.advancedCompiler.dtree.{ SimpleComposeGenerator => AdvancedDTreeGenerator }
import org.warthog.generic.parsers.DIMACSReader
import org.warthog.generic.formulas.{ Not, Formula }
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.core.MSJCoreProver

object DNNFEvaluation {

  /**
   *  Prints an evaluation of the compilation of the specified cnf to stdout
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
      if (DNNF.nodeCount2(dnnf) < 1000000 || forceCounting) println("Models: " + DNNF.countModels(dnnf, DIMACSReader.numberOfVariablesAndClauses(dimacsFile).getOrElse(-1,-1)._1))
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
