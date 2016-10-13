package de.weltraumschaf.slartibartfass.backend;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.Constants;
import de.weltraumschaf.slartibartfass.Environment;
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

    BaseExecutor(final SlartiInputOutput io) {
        super();
        this.io = Validate.notNull(io, "io");
    }

    private void loadBuiltInFunctions() {
        io.debug("Load built in function ...");
        SlartiBuiltinFunctions.register(env, io.getIo());
    }

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

    final Backend init() {
        loadBuiltInFunctions();
        loadStdLib();
        return this;
    }

    final SlartiVisitor<SlartiNode> visitor() {
        return visitor;
    }

    final void visitor(final SlartiVisitor<SlartiNode> visitor) {
        this.visitor = Validate.notNull(visitor, "visitor");
    }

    final Environment env() {
        return env;
    }

    final void env(final Environment env) {
        this.env = Validate.notNull(env, "env");
    }

    final Parsers parsers() {
        return parsers;
    }

    final void parsers(final Parsers parsers) {
        this.parsers = Validate.notNull(parsers, "parsers");
    }

    final SlartiInputOutput io() {
        return io;
    }
}
