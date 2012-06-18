/**
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

package org.warthog.pl.decisionprocedures.satsolver.impl.picosat;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;

public class JPicosat {
    private static final String DIR = "/solvers/picosat";
    private CPicosat INSTANCE;

    public interface CPicosat extends Library {
        void picosat_init();

        void picosat_reset();

        /**
         * Enables extracting cores or proof traces. This method has to be called right after the 'picosat_init' method.
         *
         * @return non-zero if code for generating traces is including and zero if traces can not be generated.
         */
        int picosat_enable_trace_generation();

        int picosat_variables();                /* p cnf <m> n */

        int picosat_added_original_clauses();   /* p cnf m <n> */

        int picosat_sat(int to);

        int picosat_deref(int int_lit);

        /**
         * Determines whether the i'th added original clause is in the unsat core.
         *
         * @param cls clause to check
         * @return non-zero if given clause is in core, otherwise zero
         */
        int picosat_coreclause(int cls);

        int picosat_add(int lit);
    }

    public JPicosat(String libdir) throws Exception {
        StringBuilder pref = new StringBuilder(libdir + DIR);

        if (Platform.isMac())
            if (Platform.is64Bit())
                pref.append("/macosx/64");
            else
                pref.append("/macosx/32");
        else if (Platform.isLinux())
            pref.append("/linux");
        else if (Platform.isWindows())
            if (Platform.is64Bit())
                pref.append("/win/64");
            else
                pref.append("/win/32");
        else
            throw new Exception("JPicosat: Platform unsupported!");

        System.setProperty("jna.library.path", pref.toString());        
        INSTANCE = (CPicosat) Native.loadLibrary("picosat", CPicosat.class);
    }

    public JPicosat() throws Exception {
        this(System.getProperty("warthog.libs"));
    }

    public void picosat_init() {
        INSTANCE.picosat_init();
    }

    public void picosat_reset() {
        INSTANCE.picosat_reset();
    }

    public int picosat_enable_trace_generation() {
        return INSTANCE.picosat_enable_trace_generation();
    }

    public int picosat_variables() {
        return INSTANCE.picosat_variables();
    }

    public int picosat_added_original_clauses() {
        return INSTANCE.picosat_added_original_clauses();
    }

    public int picosat_sat(int to) {
        return INSTANCE.picosat_sat(to);
    }

    public int picosat_deref(int int_lit) {
        return INSTANCE.picosat_deref(int_lit);
    }

    public int picosat_coreclause(int cls) {
        return INSTANCE.picosat_coreclause(cls);
    }

    public int picosat_add(int lit) {
        return INSTANCE.picosat_add(lit);
    }

    public void test() {
        picosat_init();

        picosat_add(-1);
        picosat_add(0);

        picosat_add(1);
        picosat_add(0);

        System.out.println("p cnf 1 2\n-1 0\n1 0? " + picosat_sat(-1));

        picosat_reset();

        picosat_init();

        picosat_add(1);
        picosat_add(-2);
        picosat_add(0);

        System.out.println("p cnf 2 1\n1 -2 0? " + picosat_sat(-1));

        picosat_reset();
    }

    public static void main(String[] args) throws Exception {
        //System.setProperty("warthog.libs", "/Users/ak/IdeaProjects/warthog/lib");
        JPicosat jps = new JPicosat();
        jps.test();
    }
}
