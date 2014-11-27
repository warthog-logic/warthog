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

package org.warthog.pl.decisionprocedures.satsolver.impl.picosat;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;

public class JPicosat {
    private static final String DIR = "/solvers/picosat";

    public static final int PSUNKNOWN = 0;
    public static final int PSSAT = 10;
    public static final int PSUNSAT = 20;

    private CPicosat INSTANCE;
    private Pointer currentPicosatObject;

    /**
     *  Interface for Picosat-959.
     */
    public interface CPicosat extends Library {
        Pointer picosat_init();

        void picosat_reset(Pointer object);

        int picosat_sat(Pointer object, int to);

        int picosat_variables(Pointer object);

        int picosat_deref(Pointer object, int int_lit);

        int picosat_coreclause(Pointer object, int cls);

        int picosat_add(Pointer object, int lit);

        String picosat_version();
    }

    public JPicosat(String libDir) throws Exception {
        if (libDir == null || libDir == "")
            libDir="lib";
        StringBuilder pref = new StringBuilder(libDir + DIR);

        if (Platform.isMac())
            if (Platform.is64Bit())
                pref.append("/macosx/64");
            else
                pref.append("/macosx/32");
        else if (Platform.isLinux())
            if (Platform.is64Bit())
              pref.append("/linux/64");
            else
              pref.append("/linux/32");
        else if (Platform.isWindows())
            if (Platform.is64Bit())
                pref.append("/win/64");
            else
                pref.append("/win/32");
        else
            throw new Exception("JPicosat: Platform unsupported!");

        System.setProperty("jna.library.path", pref.toString());
        INSTANCE = (CPicosat) Native.loadLibrary("picosat", CPicosat.class);
        checkLibraryVersion(pref.toString());
    }

    public JPicosat() throws Exception {
        this(System.getProperty("warthog.libs"));
    }

    public void picosat_init() {
        currentPicosatObject = INSTANCE.picosat_init();
    }

    public void picosat_reset() {
        INSTANCE.picosat_reset(currentPicosatObject);
    }

    public int picosat_sat(int to) {
        return INSTANCE.picosat_sat(currentPicosatObject, to);
    }

    public int picosat_variables() {
        return INSTANCE.picosat_variables(currentPicosatObject);
    }

    public int picosat_deref(int int_lit) {
        return INSTANCE.picosat_deref(currentPicosatObject, int_lit);
    }

    public int picosat_coreclause(int cls) {
        return INSTANCE.picosat_coreclause(currentPicosatObject, cls);
    }

    public int picosat_add(int lit) {
        return INSTANCE.picosat_add(currentPicosatObject, lit);
    }

    private void checkLibraryVersion(String path) {
      int version = 0;
      try {
        version = Integer.parseInt(INSTANCE.picosat_version());
      } catch (NumberFormatException e) { }
      if (version < 959)
        System.err.println("Warning: The picosat library provided in " + path + " seems to be too old. " +
                "Only version 959 and following are supported! The application might crash randomly.");
    }
}
