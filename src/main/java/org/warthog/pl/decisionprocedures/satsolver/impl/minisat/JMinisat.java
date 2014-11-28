/**
 * Copyright (c) 2011-2014, Andreas J. Kuebler & Christoph Zengler
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

package org.warthog.pl.decisionprocedures.satsolver.impl.minisat;

import com.sun.jna.*;

public class JMinisat {
    private static final String DIR = "/solvers/minisat";

    public static final int UNKNOWN = 0;
    public static final int SAT = 10;
    public static final int UNSAT = 20;

    private CMinisat INSTANCE;

    private interface CMinisat extends Library {
        int ms_newSolver();

        void ms_addClause(int solver, int arr[]);

        int ms_satAss(int solver, int ass[]);

        int ms_sat(int solver);

        int ms_deref(int solver, int var);

        void ms_freeSolver(int solver);
    }

    public JMinisat(String libDir) throws Exception {
        if (libDir == null || libDir == "")
            libDir = "lib";
        StringBuilder pref = new StringBuilder(libDir + DIR);

        if (Platform.isMac())
            if (Platform.is64Bit())
                pref.append("/macosx/64");
            else
                pref.append("/macosx/32");
        else if (Platform.isLinux())
            pref.append("/linux");
        else if (Platform.isWindows())
            pref.append("/win");
        else
            throw new Exception("JMinisat: Platform unsupported!");

        System.setProperty("jna.library.path", pref.toString());
        INSTANCE = (CMinisat) Native.loadLibrary("minisat", CMinisat.class);
    }

    public JMinisat() throws Exception {
        this(System.getProperty("warthog.libs"));
    }

    public int minisat_init() {
        return INSTANCE.ms_newSolver();
    }

    public int minisat_sat(int solver) {
        return INSTANCE.ms_sat(solver);
    }

    public int minisat_sat(int solver, int assumptions[]) {
        return INSTANCE.ms_satAss(solver, assumptions);
    }

    public int minisat_deref(int solver, int literal) {
        return INSTANCE.ms_deref(solver, literal);
    }

    public void minisat_add(int solver, int clause[]) {
        INSTANCE.ms_addClause(solver, clause);
    }

    public void minisat_free(int solver) {
        INSTANCE.ms_freeSolver(solver);
    }
}
