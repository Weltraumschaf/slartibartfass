package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.application.ApplicationException;
import de.weltraumschaf.commons.application.InvokableAdapter;
import de.weltraumschaf.commons.application.Version;
import de.weltraumschaf.commons.jcommander.JCommanderImproved;
import de.weltraumschaf.slartibartfass.frontend.DefaultSlartiVisitor;
import de.weltraumschaf.slartibartfass.frontend.Parsers;
import de.weltraumschaf.slartibartfass.backend.Repl;
import de.weltraumschaf.slartibartfass.frontend.SlartiParser;
import de.weltraumschaf.slartibartfass.frontend.SlartiVisitor;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.function.SlartiBuiltinFunctions;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;

/**
 * Main application class.
 * <p>
 * This class provides the {@link #main(String[])} main entry point}.
 * </p>
 *
 * @author Sven Strittmatter
 */
public final class Application extends InvokableAdapter {
    /**
     * Helper for CLI argument parsing.
     */
    private final JCommanderImproved<CliOptions> cliArgs = CliOptions.newCliArgParser();
    /**
     * Used to show the version from Maven.
     */
    private final Version version = new Version(Constants.BASE_PACKAGE.value() + "/version.properties");
    /**
     * Used for I/O.
     */
    private SlartInputOutput outup;

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
        final CliOptions opts = gatherOptions();
        prepareExecution();

        if (opts.isHelp()) {
            outup.print(opts.helpMessage(cliArgs));
            return;
        }

        if (opts.isVersion()) {
            outup.println(version.getVersion());
            return;
        }

        slarti(opts);
    }

    private CliOptions gatherOptions() {
        final CliOptions opts = cliArgs.gatherOptions(getArgs());
        debug = opts.isDebug();
        return opts;
    }

    void prepareExecution() throws IOException {
        outup = new SlartInputOutput(getIoStreams(), isDebugEnabled());
        version.load();
    }

    private void slarti(final CliOptions opts) throws ApplicationException {
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

    void loadBuiltInFunctions(final Environment env) {
        outup.debug("Load built in function ...");
        SlartiBuiltinFunctions.register(env, getIoStreams());
    }

    void loadStdLib(final SlartiVisitor<SlartiNode> visitor, final Environment env) throws IOException {
        outup.debug("Load STD lib ...");
        final InputStream src = getClass().getResourceAsStream(Constants.BASE_PACKAGE.value() + "/std-lib.sl");
        final Parsers parsers = new Parsers();
        final SlartiNode node = visitor.visit(parsers.newParser(src).file());
        node.eval(env);
    }

    private void startRepl(final SlartiVisitor<SlartiNode> visitor, final Environment env) throws IOException {
        outup.debug("Start REPL ...");
        new Repl(outup, visitor, env).start(version);
    }

    private void runInterpreter(final SlartiVisitor<SlartiNode> visitor, final Environment env, final Collection<String> filenames) throws IOException {
        for (final String filename : filenames) {
            outup.debug(String.format("Interpret file %s  ...", filename));
            final Parsers parsers = new Parsers();
            final SlartiParser parser = parsers.newParser(new FileInputStream(filename));
            visitor.visit(parser.file()).eval(env);
        }
    }

}
