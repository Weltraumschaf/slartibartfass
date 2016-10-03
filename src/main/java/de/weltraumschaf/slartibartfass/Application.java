package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.application.*;
import de.weltraumschaf.commons.jcommander.JCommanderImproved;
import de.weltraumschaf.slartibartfass.frontend.SlartiParser;
import de.weltraumschaf.slartibartfass.frontend.SlartiVisitor;
import de.weltraumschaf.slartibartfass.node.function.SlartiBuiltinFunctions;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import jline.console.ConsoleReader;
import jline.console.completer.Completer;
import jline.console.completer.NullCompleter;

import java.io.*;
import java.util.Collection;

/**
 * Main application class.
 * <p>
 *     This class provides the {@link #main(String[])} main entry point}.
 * </p>
 *
 * @author Sven Strittmatter
 */
public final class Application extends InvokableAdapter {
    /**
     * Base for accessing bundled resources.
     */
    static final String BASE_PACKAGE = "/de/weltraumschaf/slartibartfass";
    /**
     * Helper for CLI argument parsing.
     */
    private final JCommanderImproved<CliOptions> cliArgs = CliOptions.newCliArgParser();
    /**
     * USed to show the version from Maven.
     */
    private final Version version = new Version(BASE_PACKAGE + "/version.properties");
    /**
     * Dedicated constructor.
     *
     * @param args CLI arguments from JVM
     */
    Application(final String[] args) {
        super(args);
    }

    /**
     * Main entry point for the JVM.
     *
     * @param args CLI arguments from the JVM
     */
    public static void main(final String[] args) {
        InvokableAdapter.main(new Application(args));
    }

    @Override
    public void execute() throws Exception {
        version.load();
        final CliOptions opts = cliArgs.gatherOptions(getArgs());
        debug = opts.isDebug();

        if (opts.isHelp()) {
            getIoStreams().print(opts.helpMessage(cliArgs));
            return;
        }

        if (opts.isVersion()) {
            getIoStreams().println(version.getVersion());
            return;
        }

        try {
            final Environment env = new Environment();
            final SlartiVisitor<SlartiNode> visitor = new DefaultSlartiVisitor();
            loadBuiltInFunctions(env);
            loadStdLib(visitor, env);

            if (opts.getFiles().isEmpty()) {
                startRepl(visitor, env);
            } else {
                runInterpreter(visitor, env, opts.getFiles());
            }
        } catch (final Exception e) {
            final String message = e.getMessage() == null ? "No message!" : e.getMessage();
            throw new ApplicationException(ExitCodeImpl.FATAL, message, e);
        }
    }

    private void printDebug(final String msg) {
        if (isDebugEnabled()) {
            getIoStreams().println("[d] " + msg);
        }
    }

    void loadBuiltInFunctions(final Environment env) {
        printDebug("Load built in function ...");
        SlartiBuiltinFunctions.register(env, getIoStreams());
    }

    void loadStdLib(final SlartiVisitor<SlartiNode> visitor, final Environment env) throws IOException {
        printDebug("Load STD lib ...");
        final InputStream src = getClass().getResourceAsStream(BASE_PACKAGE + "/std-lib.sl");
        final Parsers parsers = new Parsers(getIoStreams());
        final SlartiNode node = visitor.visit(parsers.newParser(src, isDebugEnabled()).file());
        node.eval(env);
    }

    private void startRepl(final SlartiVisitor<SlartiNode> visitor, final Environment env) throws IOException {
        printDebug("Start REPL ...");
        new Repl(getIoStreams(), visitor, env, isDebugEnabled()).start(version);
    }

    private void runInterpreter(final SlartiVisitor<SlartiNode> visitor, final Environment env, final Collection<String> filenames) throws IOException {
        for (final String filename : filenames) {
            printDebug(String.format("Interpret file %s  ...", filename));
            final Parsers parsers = new Parsers(getIoStreams());
            final SlartiParser parser = parsers.newParser(new FileInputStream(filename), isDebugEnabled());
            visitor.visit(parser.file()).eval(env);
        }
    }

}
