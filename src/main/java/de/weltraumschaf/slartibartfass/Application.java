package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.application.ApplicationException;
import de.weltraumschaf.commons.application.InvokableAdapter;
import de.weltraumschaf.commons.application.Version;
import de.weltraumschaf.commons.jcommander.JCommanderImproved;
import de.weltraumschaf.slartibartfass.backend.Backend;
import de.weltraumschaf.slartibartfass.backend.Backends;

import java.io.IOException;

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
    private SlartiInputOutput io;

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
                io.print(opts.helpMessage(cliArgs));
                return;
            }

            if (opts.isVersion()) {
                io.println(version.getVersion());
                return;
            }

            slarti(opts);
        } catch (final Exception e) {
            final String message = e.getMessage() == null ? "No message!" : e.getMessage();
            throw new ApplicationException(ExitCodeImpl.FATAL, message, e);
        }
    }

    /**
     * Collect the CLI arguments.
     *
     * @return never {@code null}
     */
    private CliOptions gatherOptions() {
        final CliOptions opts = cliArgs.gatherOptions(getArgs());
        debug = opts.isDebug();
        return opts;
    }

    /**
     * Does everything necessary for proper execution.
     *
     * @throws IOException if version info file can't be loaded
     */
    void prepareExecution() throws IOException {
        io = new SlartiInputOutput(getIoStreams(), isDebugEnabled());
        version.load();
    }

    /**
     * Executes the concrete business logic (aka. interpret code).
     *
     * @param opts must not be {@code null}
     * @throws IOException if any file can't be loaded
     */
    private void slarti(final CliOptions opts) throws IOException {
        final Backends backends = new Backends();
        final Backend backend;

        if (opts.getFiles().isEmpty()) {
            io.debug("Start REPL ...");
            backend = backends.newRepl(io, version);
        } else {
            io.debug("Start Interpreter ...");
            backend = backends.newInterpreter(io, opts.getFiles());
        }

        backend.start();
    }

}
