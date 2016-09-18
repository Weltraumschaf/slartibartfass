package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.system.ExitCode;

public enum ExitCodeImpl implements ExitCode {
    /**
     * Any unspecified error.
     */
    FATAL(255),
    /**
     * No errors.
     */
    OK(0);

    /**
     * Exit code number returned as exit code to JVM.
     */
    private final int code;

    /**
     * Dedicated constructor.
     *
     * @param code exit code number
     */
    ExitCodeImpl(final int code) {
        this.code = code;
    }

    @Override
    public int getCode() {
        return code;
    }
}
