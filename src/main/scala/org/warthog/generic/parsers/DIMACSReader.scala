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

package org.warthog.generic.parsers

import org.warthog.generic.formulas._
import org.warthog.pl.formulas.PL
import org.warthog.pl.datastructures.cnf.{ PLLiteral, ImmutablePLClause }
import org.warthog.fol.formulas._
import org.warthog.fol.datastructures.cnf.{ FOLLiteral, ImmutableFOLClause }
import org.warthog.fol.formulas.FOLVariable
import scala.Some
import org.warthog.pl.formulas.PLAtom

/**
  * A Reader for dimacs- and qdimacs-files
  * There are methods for reading a (q)dimacs-file and returning:
  * - a List[Set[Int]]
  * - a Formula[PL]
  * - a List[ImmutablePLClause]
  *
  * - a Formula[FOL]
  * - a (List[(String, Set[Int])], List[ImmutableFOLClause])
  *
  * TODO: Read also dnf
  */
object DIMACSReader {

  /**
    * Reads a dimacs-file and returns a corresponding Formula[PL]
    *
    * @param path The path to the dimacs-file
    * @return A corresponding Formula[PL]
    */
  def dimacs2Formula(path: String): Formula[PL] =
    And(dimacs2Clauses(path).map(
      cls => Or(cls.toList.map(lit =>
        if (lit < 0) Not(PLAtom(math.abs(lit).toString)) else PLAtom(lit.toString)): _*)): _*)

  /**
    * Reads a dimacs-file and returns a corresponding List of ImmutablePLClauses
    *
    * @param path The path to the dimacs-file
    * @return A corresponding List[ImmutablePLClause]
    */
  def dimacs2PLClauses(path: String): List[ImmutablePLClause] =
    dimacs2Clauses(path).map(cls =>
      new ImmutablePLClause(cls.toList.map(lit =>
        PLLiteral(math.abs(lit).toString, lit > 0))))

  /**
    * Reads a dimacs-file and returns the formula
    * as list of clauses (represented as sets of ints)
    * Throws an exception if the file is actually a qdimacs-file
    *
    * @param path The path to the dimacs-file
    * @return A corresponding list of clauses (list of set of int)
    */
  def dimacs2Clauses(path: String): List[Set[Int]] = parseDimacs(path) match {
    case (None, result) => result
    case (_, _)         => throw new Exception("Expected dimacs-file, found Qdimacs-file!")
  }

  /**
    * Reads a qdimacs-file and returns a corresponding Formula[FOL]
    *
    * @param path The path to the qdimacs-file
    * @return A corresponding Formula[FOL]
    */
  def qdimacs2Formula(path: String): Formula[FOL] = parseDimacs(path) match {
    case (None, _) => throw new Exception("How should we handle that?")
    case (Some(quants), clauses) =>
      val matrix = And(clauses.map(cls => Or(cls.toList.map(lit =>
        if (lit > 0) FOLPredicate(math.abs(lit).toString) else -FOLPredicate(math.abs(lit).toString)): _*)): _*)
      quants.foldRight(matrix.asInstanceOf[Formula[FOL]])((quant, formula) => quant._1 match {
        case Formula.EXISTS => FOLExists(quant._2.map(v => FOLVariable(v.toString)), formula)
        case Formula.FORALL => FOLForAll(quant._2.map(v => FOLVariable(v.toString)), formula)
      })
  }

  /**
    * Reads a qdimacs-file and returns a tupel:
    *  - The first element contains a list of tupels representing the quantifications as described in parseDimacs-method
    *  - The second element contains the list of clauses
    *
    * @param path The path to the qdimacs-file
    * @return The result
    */
  def qdimacs2FOLClauses(path: String): (List[(String, Set[Int])], List[ImmutableFOLClause]) = parseDimacs(path) match {
    case (None, _) => throw new Exception("How should we handle that?")
    case (Some(quants), clauses) =>
      val folClauses = clauses.map(cls => new ImmutableFOLClause(cls.toList.map(lit =>
        FOLLiteral(FOLPredicate(math.abs(lit).toString), lit > 0))))
      (quants, folClauses)
  }

  /**
    * Reads a dimacs- or qdimacs-file and returns
    *  - case dimacs: a tupel (None, the formula as list of clauses (represented as sets of ints))
    *  - case qdimacs: a tupel:
    *        + the first element contains a list of tupels representing the quantifications
    *              (a String which is either Formula.EXISTS or Formula.FORALL, and a list of Ints for the quantified variables)
    *        + the second element contains the list of clauses (as sets of ints)
    *
    *  If the file contains more than one preamble,
    * or the actual number of clauses/variables doesn't
    * correspond to the preamble, a message will be printed
    * to StdErr and the result be returned (ignoring the preamble)
    *
    * @param path The path to the dimacs-file
    * @return The result
    */
  private def parseDimacs(path: String): (Option[List[(String, Set[Int])]], List[Set[Int]]) = {

    var preambleRead = false
    var numberOfClausesInPreamble = 0
    var numberOfVarsInPreamble = 0

    val lines = io.Source.fromFile(path).getLines()
    var lineNumber = 0

    var quantifiers = List[(String, Set[Int])]()

    var vars = Set[Int]()
    var clauses = List[Set[Int]]()
    var currentClause = Set[Int]()

    while (lines.hasNext) {
      lineNumber += 1
      val line = lines.next().trim.replaceAll("\\s+", " ")

      if (!line.isEmpty) {
        line(0) match {
          case 'c' => ()
          case 'p' =>
            if (preambleRead)
              System.err.println("Line " + lineNumber + ": More than one preamble --> Use the first")
            else {
              val tokens = line.split(" ")
              if (tokens.size == 4) {
                if (tokens(1).toLowerCase == "cnf")
                  try {
                    numberOfVarsInPreamble = tokens(2).toInt
                    numberOfClausesInPreamble = tokens(3).toInt
                    preambleRead = true
                  } catch {
                    case e: NumberFormatException =>
                      System.err.println("Line " + lineNumber + ": Number format exception in preamble --> Skip line")
                      numberOfClausesInPreamble = 0
                      numberOfVarsInPreamble = 0
                  }
                else {
                  System.err.println("Line " + lineNumber + ": No cnf specified --> Skip line")
                }
              } else
                System.err.println("Line " + lineNumber + ": Not 4 tokens in preamble --> Skip line")
            }
          case 'e' =>
            quantifiers :+= (Formula.EXISTS, line.split(" ").tail.map(_.toInt).filterNot(_ == 0).toSet)
          case 'a' =>
            quantifiers :+= (Formula.FORALL, line.split(" ").tail.map(_.toInt).filterNot(_ == 0).toSet)
          case _ =>
            try {
              currentClause ++= line.split(" ").map(_.toInt).toSet
            } catch {
              case e: NumberFormatException => System.err.println("Line " + lineNumber + ": Number format exception --> Skip literal and rest of line")
            }
            if (currentClause.contains(0)) {
              currentClause = currentClause.filterNot(_ == 0)
              vars ++= currentClause.map(_.abs)
              clauses :+= currentClause
              currentClause = Set[Int]()
            }
        }
      }
    }

    if (preambleRead) {
      if (numberOfClausesInPreamble != clauses.size)
        System.err.println("WARNING: Number of Clauses in Preamble: " + numberOfClausesInPreamble + ", "
          + "Number of Clauses found: " + clauses.size)
      if (numberOfVarsInPreamble != vars.size)
        System.err.println("WARNING: Number of Vars in Preamble: " + numberOfVarsInPreamble + ", "
          + "Number of Vars found: " + vars.size)
    }

    if (quantifiers.isEmpty) // dimacs
      (None, clauses)
    else { //qdimacs
      val unquantified = vars filterNot (quantifiers.map(_._2).flatten.contains(_))
      if (!unquantified.isEmpty) quantifiers +:= (Formula.EXISTS, unquantified) // prepend

      (Some(quantifiers), clauses)
    }
  }

  /**
    * Returns the number of Variables and Clauses according to the Preamble
    * @param path The path to the dimacs-file
    * @return A tupel (#Variables,#Clauses)
    */
  def numberOfVariablesAndClauses(path: String): Option[(Int, Int)] = {
    val lines = io.Source.fromFile(path).getLines()
    lines.find(_(0) == 'p') match {
      case None => None
      case Some(line) =>
        val tokens = line.trim.replaceAll("\\s+", " ").split(" ")
        try {
          Some(tokens(2).toInt, tokens(3).toInt)
        } catch {
          case _: Throwable => None
        }
    }
  }
}