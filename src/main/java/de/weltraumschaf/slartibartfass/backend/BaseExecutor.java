package de.weltraumschaf.slartibartfass.backend;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.Constants;
import de.weltraumschaf.slartibartfass.SlartiInputOutput;
import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.frontend.DefaultSlartiVisitor;
import de.weltraumschaf.slartibartfass.frontend.Parsers;
import de.weltraumschaf.slartibartfass.frontend.SlartiVisitor;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.function.SlartiBuiltinFunctions;

import java.io.IOException;
import java.io.InputStream;

/**
 * Common functionality for {@link Backend backends}.
 *
 * @author Sven Strittmatter
 */
abstract class BaseExecutor implements Backend {
    /**
     * Visitor to convert the parsed input.
     */
    private  SlartiVisitor<SlartiNode> visitor = new DefaultSlartiVisitor();
    /**
     * The root scope to allocate memory.
     */
    private  Environment env = new Environment();
    /**
     * Factory to create a parser.
     */
    private  Parsers parsers = new Parsers();
    /**
     * Injected I/O.
     */
    private final SlartiInputOutput io;

    /**
     * Dedicated constructor.
     *
     * @param io must not be {@code null}
     */
    BaseExecutor(final SlartiInputOutput io) {
        super();
        this.io = Validate.notNull(io, "io");
    }

    /**
     * Load built in functions into environment.
     */
    private void loadBuiltInFunctions() {
        io.debug("Load built in function ...");
        SlartiBuiltinFunctions.register(env, io.getIo());
    }

    /**
     * Load standard library functions into environment.
     */
    private void loadStdLib() {
        io.debug("Load STD lib ...");
        final InputStream src = getClass().getResourceAsStream(Constants.BASE_PACKAGE.value() + "/std-lib.sl");
        final SlartiNode node;

        try {
            node = visitor.visit(parsers.newParser(src).file());
        } catch (final IOException e) {
            throw new SlartiError("Can't read STD lib! Reason: %s", e.getMessage());
        }

        node.eval(env);
    }

    /**
     * Initialize the executor and must be called before first use.
     *
     * @return self for chaining
     */
    final Backend init() {
        loadBuiltInFunctions();
        loadStdLib();
        return this;
    }

    /**
     * Get a visitor to traverse the AST.
     *
     * @return never {@code null}
     */
    final SlartiVisitor<SlartiNode> visitor() {
        return visitor;
    }

    /**
     * Injects a visitor to traverse the AST.
     *
     * @param visitor must not be {@code null}
     */
    final void visitor(final SlartiVisitor<SlartiNode> visitor) {
        this.visitor = Validate.notNull(visitor, "visitor");
    }

    /**
     * Get the top level scope.
     *
     * @return never {@code null}
     */
    final Environment env() {
        return env;
    }

    /**
     * Inject new top level scope.
     *
     * @param env must not be {@code null
     */
    final void env(final Environment env) {
        this.env = Validate.notNull(env, "env");
    }

    /**
     * Get parser facotry to create source code parsers.
     *
     * @return never {@code null}
     */
    final Parsers parsers() {
        return parsers;
    }

    /**
     * Inject parser factory.
     *
     * @param parsers must not be {@code null
     */
    final void parsers(final Parsers parsers) {
        this.parsers = Validate.notNull(parsers, "parsers");
    }

    /**
     * Get the I/O.
     *
     * @return never {@code null}
     */
    final SlartiInputOutput io() {
        return io;
    }
}
