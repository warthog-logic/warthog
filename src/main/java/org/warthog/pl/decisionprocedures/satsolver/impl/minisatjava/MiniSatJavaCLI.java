/******************************************************************************************
 MiniSat -- Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson

 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 associated documentation files (the "Software"), to deal in the Software without restriction,
 including without limitation the rights to use, copy, modify, merge, publish, distribute,
 sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all copies or
 substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
 OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 **************************************************************************************************
 Remarks:
 * The following source code is basically a Java conversion of the C/C++ MiniSAT v1.14
 **************************************************************************************************/

package org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava;

import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.collections.nativeType.IntVec;
import org.warthog.pl.decisionprocedures.satsolver.impl.minisatjava.prover.core.MSJCoreProver;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.StringTokenizer;

/**
 * Command line interface for MiniSATJava
 */
public class MiniSatJavaCLI {

  public static void main(String[] args) {
    assert args.length > 1 : "Not enough arguments";
    try {
      MSJCoreProver prover = new MSJCoreProver();
      FileReader reader = new FileReader(args[0]);
      BufferedReader in = new BufferedReader(reader);
      readDimacs(in, prover);
      long start = System.currentTimeMillis();
      boolean res = prover.solve();
      long end = System.currentTimeMillis();
      double cpu_time = (end - start) / 1000.0;
      System.out.printf("restarts              : %d\n", prover.stats.starts);
      System.out.printf("conflicts             : %-12d   (%.0f /sec)\n", prover.stats.conflicts,
              (prover.stats.conflicts / cpu_time));
      System.out.printf("decisions             : %-12d   (%.0f /sec)\n", prover.stats.decisions,
              prover.stats.decisions / cpu_time);
      System.out.printf("propagations          : %-12d   (%.0f /sec)\n", prover.stats.propagations,
              prover.stats.propagations / cpu_time);
      System.out.printf("conflict literals     : %-12d   (%4.2f %% deleted)\n", prover.stats.tot_literals,
              (prover.stats.max_literals - prover.stats.tot_literals) * 100 / (double) prover.stats.max_literals);
      System.out.printf("CPU time              : %g s\n", cpu_time);
      System.out.println("\n" + (res ? "SATISFIABLE" : "UNSATISFIABLE"));
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  public static boolean solve(String file) throws Exception {
    MSJCoreProver prover = new MSJCoreProver();
    FileReader reader = new FileReader(file);
    BufferedReader in = new BufferedReader(reader);
    readDimacs(in, prover);
    return prover.solve();
  }

  private static void readDimacs(BufferedReader br, MSJCoreProver prover) throws Exception {
    int numberOfVarsInPreamble = 0;
    boolean preambleRead = false;
    IntVec clause = new IntVec();
    int clauseCounter = 0;
    int lineNumber = 0;
    String line = null;
    while ((line = br.readLine()) != null) {
      lineNumber++;
      // ignore empty lines
      if (line.length() == 0) {
        continue;
      }
      // ignore comments (line starts with c)
      if (line.startsWith("c")) {
        continue;
      }
      // read preamble (line starts with p)
      if (line.startsWith("p")) {
        if (preambleRead) {
          System.err.println("Line " + lineNumber + ": More than one preamble --> Use the first");
          continue;
        }
        StringTokenizer st = new StringTokenizer(line);
        int tokens = st.countTokens();
        if (tokens == 4) {
          try {
            int count = 0;
            while (st.hasMoreTokens()) {
              count++;
              String t = st.nextToken();
              if (count == 3) {
                numberOfVarsInPreamble = Integer.parseInt(t);
              }
            }
            preambleRead = true;
            for (int i = 0; i < numberOfVarsInPreamble; i++)
              prover.newVar();
          } catch (NumberFormatException e) {
            System.err.println("Line " + lineNumber + ": Number format exception in preamble --> Skip line");
          }
        } else {
          System.err.println("Line " + lineNumber + ": Not 4 tokens in preamble --> Skip line");
        }
      } else {
        // read clause, read to '0' (must not be in one line)
        try {
          StringTokenizer st = new StringTokenizer(line);
          while (st.hasMoreElements()) {
            String t = st.nextToken();
            int var = Integer.parseInt(t);
            if (var == 0) {
              // end of clause reached
              prover.newClause(clause, false);
              clauseCounter++;
              clause = new IntVec();
            } else if (var < 0) {
              clause.push((((var * -1) - 1) * 2) ^ 1);
            } else {
              clause.push((var - 1) * 2);
            }
          }
        } catch (NumberFormatException e) {
          //System.err.println("Line " + lineNumber + ": Number format exception --> Skip literal and rest of line");
        }
      }
    }
    br.close();
  }
}
