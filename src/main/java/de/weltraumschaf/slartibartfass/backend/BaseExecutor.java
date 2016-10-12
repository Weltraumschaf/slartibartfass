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
    final SlartiVisitor<SlartiNode> visitor = new DefaultSlartiVisitor();
    /**
     * The root scope to allocate memory.
     */
    protected final Environment env = new Environment();
    /**
     * Factory to create a parser.
     */
    final Parsers parsers = new Parsers();
    /**
     * Injected I/O.
     */
    final SlartiInputOutput io;

    public BaseExecutor(final SlartiInputOutput io) {
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
}
