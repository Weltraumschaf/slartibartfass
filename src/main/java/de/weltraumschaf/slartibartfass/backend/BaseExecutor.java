package de.weltraumschaf.slartibartfass.backend;

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
 * @author Sven Strittmatter
 */
abstract class BaseExecutor {
    /**
     * Visitor to convert the parsed input.
     */
    protected final SlartiVisitor<SlartiNode> visitor = new DefaultSlartiVisitor();
    /**
     * The root scope to allocate memory.
     */
    protected final Environment env = new Environment();
    protected final Parsers parsers = new Parsers();
    /**
     * Injected I/O.
     */
    protected final SlartiInputOutput output;

    public BaseExecutor(final SlartiInputOutput output) {
        super();
        this.output = output;
    }

    private void loadBuiltInFunctions() {
        output.debug("Load built in function ...");
        SlartiBuiltinFunctions.register(env, output.getIo());
    }

    private void loadStdLib() {
        output.debug("Load STD lib ...");
        final InputStream src = getClass().getResourceAsStream(Constants.BASE_PACKAGE.value() + "/std-lib.sl");
        final SlartiNode node;

        try {
            node = visitor.visit(parsers.newParser(src).file());
        } catch (final IOException e) {
            throw new SlartiError("Can't read STD lib! Reason: %s", e.getMessage());
        }

        node.eval(env);
    }

    protected final void init() {
        loadBuiltInFunctions();
        loadStdLib();
    }
}
