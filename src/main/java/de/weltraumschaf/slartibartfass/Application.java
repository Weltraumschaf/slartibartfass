package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.application.*;
import de.weltraumschaf.commons.jcommander.JCommanderImproved;
import de.weltraumschaf.slartibartfass.frontend.SlartiParser;
import de.weltraumschaf.slartibartfass.frontend.SlartiVisitor;
import de.weltraumschaf.slartibartfass.node.function.SlartiBuiltinFunctions;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

import java.io.*;
import java.util.Collection;

public class Application extends InvokableAdapter {
    static final String BASE_PACKAGE = "/de/weltraumschaf/slartibartfass";

    private final JCommanderImproved<CliOptions> cliArgs = new JCommanderImproved<>(CliOptions.PROG_NAME, CliOptions.class);
    private final Parsers parsers;

    Application(final String[] args, final IO io) {
        super(args);
        this.parsers = new Parsers(io);
    }

    public static void main(final String[] args) throws UnsupportedEncodingException {
        InvokableAdapter.main(new Application(args, IOStreams.newDefault()));
    }

    @Override
    public void execute() throws Exception {
        final CliOptions opts = cliArgs.gatherOptions(getArgs());
        debug = opts.isDebug();

        if (opts.isHelp()) {
            getIoStreams().print(cliArgs.helpMessage(CliOptions.USAGE, CliOptions.DESCRIPTION, CliOptions.EXAMPLES));
            return;
        }

        if (opts.isVersion()) {
            final Version version = new Version(BASE_PACKAGE + "/version.properties");
            version.load();
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
            throw new ApplicationException(ExitCodeImpl.FATAL, e.getMessage(), e);
        }
    }

    private void printDebug(final String msg) {
        if (isDebugEnabled()) {
            getIoStreams().println("[d] " + msg);
        }
    }

    void loadBuiltInFunctions(final Environment env) {
        printDebug("Load built in function ...");
        SlartiBuiltinFunctions.setIo(getIoStreams());
        SlartiBuiltinFunctions.register(env);
    }

    public void loadStdLib(final SlartiVisitor<SlartiNode> visitor, final Environment env) throws IOException {
        printDebug("Load STD lib ...");
        final InputStream src = getClass().getResourceAsStream(BASE_PACKAGE + "/std-lib.sl");
        final SlartiNode node = visitor.visit(parsers.newParser(src, isDebugEnabled()).file());
        node.eval(env);
    }

    private void startRepl(final SlartiVisitor<SlartiNode> visitor, final Environment env) throws IOException {
        printDebug("Start REPL ...");
        final Console console = System.console();

        while (true) {
            final String data = console.readLine("~> ");

            if (data == null) {
                // EOF sent
                break;
            }

            if ("!env".equals(data)) {
                env.print(getIoStreams().getStdout());
                continue;
            }

            try {
                final SlartiParser parser = parsers.newParser(new ByteArrayInputStream(data.getBytes()), isDebugEnabled());
                final Object result = visitor.visit(parser.file()).eval(env);
                getIoStreams().println(result.toString());
            } catch (final SlartiError e) {
                getIoStreams().errorln("[E] " + e.getMessage());
            }
        }
    }

    private void runInterpreter(final SlartiVisitor<SlartiNode> visitor, final Environment env, final Collection<String> filenames) throws IOException {
        for (final String filename : filenames) {
            printDebug(String.format("Interpret file %s  ...", filename));
            final SlartiParser parser = parsers.newParser(new FileInputStream(filename), isDebugEnabled());
            visitor.visit(parser.file()).eval(env);
        }
    }

}
