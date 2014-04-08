package org.warthog.pl.knowledgecompilation.dnnf

import org.warthog.pl.io.CNFUtil
import org.warthog.pl.formulas.{ PLAtom, PL }
import collection.immutable.HashMap
import org.warthog.pl.knowledgecompilation.dnnf.simpleCompiler.dtree.{ Generator => SimpleDTreeGenerator }
import org.warthog.pl.knowledgecompilation.dnnf.advancedCompiler.dtree.{ SimpleComposeGenerator => AdvancedDTreeGenerator }
import org.warthog.pl.knowledgecompilation.dnnf.advancedCompiler.dtree.{ C2DDTreeGenerator => AdvancedC2DDTreeGenerator }
import org.warthog.pl.knowledgecompilation.dnnf.simpleCompiler.SimpleDNNFCompiler
import org.warthog.pl.knowledgecompilation.dnnf.advancedCompiler.AdvancedDNNFCompiler
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
object DNNFCompilation {

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
  def compileWithC2DDTree(dimacsFile: String): DNNF = {
    val clauses = DIMACSReader.dimacs2Clauses(dimacsFile)
    val solverClauses = clauses map (_.map(v => MSJCoreProver.mkLit(math.abs(v), v < 0)))
    val dtree = AdvancedC2DDTreeGenerator.generateDTree(dimacsFile)

    val compiler = new AdvancedDNNFCompiler(clauses.size, dtree.varSet.size)
    if (!compiler.initSolver(solverClauses))
      False
    else {
      if (verbose)
        println("Starting DTree Compilation")
      val dnnf = compiler.cnf2dnnf(dtree)
      if (verbose)
        println("---\nRecursive Calls: " + compiler.recursiveCalls + "\nCache Hits: " + compiler.cacheHits)
      val result = DNNF.simplify(dnnf)
      result
    }
  }


}
