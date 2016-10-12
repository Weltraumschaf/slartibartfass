package de.weltraumschaf.slartibartfass.backend;

import java.io.IOException;

/**
 * Implementations work on the parsed AST.
 *
 * @author Sven Strittmatter
 */
public interface Backend {
    /**
     * Starts the backend.
     * <p>
     * May throw {@link de.weltraumschaf.slartibartfass.SlartiError} if something fails.
     * </p>
     *
     * @throws IOException if the REPL can't read from the console
     */
    void start() throws IOException;
}
