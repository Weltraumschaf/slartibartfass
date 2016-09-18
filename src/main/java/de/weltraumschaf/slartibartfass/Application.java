package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.application.ApplicationException;
import de.weltraumschaf.commons.application.IOStreams;
import de.weltraumschaf.commons.application.InvokableAdapter;
import de.weltraumschaf.slartibartfass.frontend.SlartiParser;
import de.weltraumschaf.slartibartfass.frontend.SlartiVisitor;
import de.weltraumschaf.slartibartfass.node.function.SlartiBuiltinFunction;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

import java.io.*;
import java.util.Collection;
import java.util.List;

public class Application extends InvokableAdapter {
    private static final boolean DEBUG = true;
    private final PrintStream out = System.out;
    private final PrintStream err = System.err;
    private final Environment env = new Environment();
    private final SlartiVisitor<SlartiNode> visitor = new DefaultSlartiVisitor();
    private final Parsers parsers;
    private final String[] args;

    public Application(final String[] args) {
        super(args);
        this.args = args;
        this.parsers = new Parsers(out, err);
        SlartiBuiltinFunction.register(env);
    }

    public static void main(final String[] args) {
        InvokableAdapter.main(new Application(args));
    }

    @Override
    public void execute() throws Exception {
        if (args.length > 1) {
            throw new ApplicationException(
                ExitCodeImpl.FATAL,
                "Slartibartfass only accepts either no argument or one file as argument!");
        }

        try {
            loadStdLib();

            if (args.length == 0) {
                startRepl();
            } else {
                runInterpreter(args[0]);
            }
        } catch (final Exception e) {
            throw new ApplicationException(ExitCodeImpl.FATAL, e.getMessage(), e);
        }
    }

    private void loadStdLib() throws IOException {
        if (DEBUG) {
            out.println("[d] Load STD lib ...");
        }

        final InputStream src = getClass().getResourceAsStream("/de/weltraumschaf/slartibartfass/std-lib.sl");
        visitor.visit(parsers.newParser(src, DEBUG).file()).eval(env);
    }

    private void startRepl() throws IOException {
        if (DEBUG) {
            out.println("[d] Start REPL ...");
        }

        final Console console = System.console();

        while (true) {
            final String data = console.readLine("~> ");

            if (data == null) {
                // EOF sent
                break;
            }

            final SlartiParser parser = parsers.newParser(new ByteArrayInputStream(data.getBytes()), DEBUG);
            final Object result = visitor.visit(parser.file()).eval(env);

            if (result != SlartiList.EMPTY) {
                out.println(result);
            }
        }
    }

    private void runInterpreter(final String filename) throws IOException {
        if (DEBUG) {
            out.println(String.format("[d] Interpret file %s  ...", filename));
        }

        final SlartiParser parser = parsers.newParser(new FileInputStream(filename), DEBUG);
        visitor.visit(parser.file()).eval(env);
    }

}
