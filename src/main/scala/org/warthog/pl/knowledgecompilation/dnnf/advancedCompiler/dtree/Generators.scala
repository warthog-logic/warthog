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

import org.warthog.pl.knowledgecompilation.dnnf._
import scala.sys.process.{ProcessLogger, Process}
import com.sun.jna.Platform
import java.io.File
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.core.MSJCoreProver

/**
 * Contains methods for generating DTrees (advanced compiler)
 */
trait Generator

object SimpleComposeGenerator extends Generator {

  /**
   * Generates a DTree using a primitive algorithm described in
   * A. Darwiche "Decomposable Negation Normal Form"
   *
   * @param clauses The list of clauses
   * @return A dtree containing the clauses in its leafs
   */
  def generateDTree(clauses: List[Set[Int]], reverseCompose: Boolean = true): DTree = {
    val pi = clauses.toSet.flatten
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

    compose(sigma, reverseCompose).head
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

object C2DDTreeGenerator extends Generator {
  /**
   * Takes a dimacs-file and generates the dtree using the c2d-compiler.
   * Since c2d uses HyperGraph-Partitioning to produce the dtree
   * this dtree will let the compiler work orders of magnitude faster
   * @param dimacsFile The dimacs-File. Each clause in this file MUST be defined in just ONE line!
   * @return The dtree
   */
  def generateDTree(dimacsFile: String): DTree = {
    val dtreeFile = dimacsFile + ".dtree"
    if (!new File(dtreeFile).exists())
      createDTreeWithC2D(dimacsFile)
    readFromC2DFile(dimacsFile)
  }

  /**
   * Reads a c2dDtreeFile that it assumes to be called @code{dimacsFile + ".dtree"}
   * @param dimacsFile The dimacsFile. The corresponding c2dDTreeFile is assumed to be called @code{dimacsFile + ".dtree"}
   * @return The dtree
   */
  def readFromC2DFile(dimacsFile: String): DTree = {
    val dtreeFile = dimacsFile + ".dtree"
    val dTreeLines = io.Source.fromFile(dtreeFile).getLines.drop(1).toArray
    val dimacsLines = io.Source.fromFile(dimacsFile).getLines.
      map(_.trim).map(_.replaceAll("\\s+", " ")).
      filterNot(l => l.isEmpty | l.startsWith("c") | l.startsWith("p")).
      toArray

    val dTreeTmp = new Array[DTree](dTreeLines.size)
    for (i <- 0 until dTreeLines.size)
      dTreeTmp(i) = dTreeLines(i)(0) match {
        case 'L' =>
          val n = dTreeLines(i).split(" ")(1).toInt
          DTreeLeaf(n, dimacsLines(n).split(" ").map(_.toInt).filterNot(_ == 0).map(v => MSJCoreProver.mkLit(math.abs(v), v < 0)).toSet)
        case 'I' =>
          val ns = dTreeLines(i).split(" ")
          DTreeNode(dTreeTmp(ns(1).toInt), dTreeTmp(ns(2).toInt))
        case _ => throw new Exception("")
      }
    dTreeTmp.last
  }

  /**
   * Uses the c2d-Compiler to produce a DTree-file from the specified dimacs-file
   * The resulting file will be in "'dimacsFile'.dtree"
   * (The c2d-Compiler will be interrupted after having created the dtree,
   * since there is no way to only produce the dtree-file without computing the dnnf)
   * @param dimacsFile The dimacs-file for which the dtree should be created
   * @param method The method to use for DTree-Generation, the default value is 0 which will usually yield the best results
   *               Further information can be accessed in the c2d-manual
   * @return Unit -- the resulting file will be in "'dimacsFile'.dtree"
   */
  /* TODO: Test this method on linux */
  def createDTreeWithC2D(dimacsFile: String, method: Int = 4): Unit = {
    var p: Process = null
    var dtreeCreated = false
    val log = new StringBuilder

    /** The ProcessLogger will forward the stdout-lines to this method, which will interrupt the process after the dtree is created */
    def isFinished(line: String): Unit = {
      if (line.matches(".*Saving dtree.*done.*")) {
        dtreeCreated = true
        p.destroy
      } else {
        log.append(line + "\n")
      }
    }

    // Create the path to the c2d-compiler
    val sep = "/"
    //val lib = System.getProperty("warthog.libs") + sep + "dnnf" + sep + "c2d" + sep
    val lib = "lib/dnnf/c2d/"
    val c2d =
      if (Platform.isLinux())
        lib + "linux" + sep + "c2d"
      else if (Platform.isWindows())
        lib + "win" + sep + "c2d.exe"
      else
        throw new Exception("c2d: Platform unsupported!")

    // Create and execute the process
    val builder = Process(List(c2d, "-in", dimacsFile, "-dt_method", method.toString, "-dt_out"))
    val logger = ProcessLogger(isFinished(_))
    p = builder.run(logger)

    val exit = p.exitValue // wait until process is determinated
    if (!dtreeCreated)
      throw new Exception("DTree could not be created! Exit-Value " + exit + "\n\n c2d-output:\n" + log)
    else if (verbose) println("DTree created.")
  }

}
