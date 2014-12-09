/*
 * Copyright (c) 2011-2014, Andreas J. Kuebler & Christoph Zengler & Rouven Walter
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

package org.warthog.pl.parsers.maxsat

import collection.mutable.ListBuffer
import org.warthog.pl.datastructures.cnf.{PLLiteral, ImmutablePLClause}
import io.Source

/**
 * A Reader for partial MaxSAT files.
 *
 * Reference: http://maxsat.ia.udl.cat/requirements/
 */
class PartialMaxSATReader {
  var topWeight: Long = -1
  var hardClauses = new ListBuffer[ImmutablePLClause]
  var softClauses = new ListBuffer[ImmutablePLClause]

  private def reset() {
    topWeight = -1
    hardClauses.clear()
    softClauses.clear()
  }

  def read(filePath: String) {
    reset()
    Source.fromFile(filePath).getLines().foreach(line => processLine(line.trim()))
  }

  private def processLine(line: String) {
    line.trim()(0) match {
      case 'c' => // comment line
      case 'p' => processProblemLine(line)
      case _ => processClauseLine(line)
    }
  }

  private def processProblemLine(line: String) {
    val parts = line.split("\\s+")
    topWeight = parts(4).toLong
  }

  private def processClauseLine(line: String) {
    val parts = line.split("\\s+")
    val clause = new ImmutablePLClause(parts.drop(1).filter(_ != "0").map(_.toInt).map(PLLiteral(_)).toList)
    if (isHardClause(parts))
      hardClauses += clause
    else
      softClauses += clause
  }

  private def isHardClause(parts: Array[String]) = parts(0).toLong == topWeight
}
