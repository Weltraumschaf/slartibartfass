package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.application.ApplicationException;
import de.weltraumschaf.commons.application.InvokableAdapter;
import de.weltraumschaf.commons.application.Version;
import de.weltraumschaf.commons.jcommander.JCommanderImproved;
import de.weltraumschaf.slartibartfass.backend.Interpreter;
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
    private SlartInputOutput output;

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
        try {
            final CliOptions opts = gatherOptions();
            prepareExecution();

            if (opts.isHelp()) {
                output.print(opts.helpMessage(cliArgs));
                return;
            }

            if (opts.isVersion()) {
                output.println(version.getVersion());
                return;
            }

            slarti(opts);
        } catch (final Exception e) {
            final String message = e.getMessage() == null ? "No message!" : e.getMessage();
            throw new ApplicationException(ExitCodeImpl.FATAL, message, e);
        }
    }

    private CliOptions gatherOptions() {
        final CliOptions opts = cliArgs.gatherOptions(getArgs());
        debug = opts.isDebug();
        return opts;
    }

    void prepareExecution() throws IOException {
        output = new SlartInputOutput(getIoStreams(), isDebugEnabled());
        version.load();
    }

    private void slarti(final CliOptions opts) throws IOException {
        if (opts.getFiles().isEmpty()) {
            startRepl();
        } else {
            runInterpreter(opts.getFiles());
        }
    }

    private void startRepl() throws IOException {
        output.debug("Start REPL ...");
        new Repl(output).start(version);
    }

    private void runInterpreter(final Collection<String> filenames) {
        output.debug("Start Interpreter ...");
        new Interpreter(output).start(filenames);
    }

}
