package de.weltraumschaf.slartibartfass.backend;

import de.weltraumschaf.commons.application.Version;
import de.weltraumschaf.slartibartfass.SlartiInputOutput;

import java.util.Collection;

/**
 * Factory to create backends.
 *
 * @author Sven Strittmatter
 */
public final class Backends {

    /**
     * Creates a new REPL.
     *
     * @param io must not be {@code null}
     * @param version must not be {@code null}
     * @return never {@code null}, always new instance
     */
    public Backend newRepl(final SlartiInputOutput io, final Version version) {
        return new Repl(io, version).init();
    }

    /**
     * Creates a new interpreter.
     *
     * @param io must not be {@code null}
     * @param filenames must not be {@code null}
     * @return never {@code null}, always new instance
     */
    public Backend newInterpreter(final SlartiInputOutput io, final Collection<String> filenames) {
        return new Interpreter(io, filenames).init();
    }
}
