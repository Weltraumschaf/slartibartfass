package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.slartibartfass.node.function.SlartiBuiltinFunction;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

import java.io.*;
import java.util.Collection;

public class Application {
    private final PrintStream out = System.out;
    private final PrintStream err = System.err;
    private final Parser parser = new Parser();
    private final Environment env = new Environment();
    private final String[] args;

    public Application(final String[] args) {
        super();
        this.args = args;
        SlartiBuiltinFunction.register(env);
    }

    public static void main(final String[] args) {
        System.exit(new Application(args).run());
    }

    private int run() {
        if (args.length > 1) {
            err.println("Slartibartfass only accepts either no argument or one file as argument!");
            return 255;
        }

        try {
            if (args.length == 0) {
                startRepl();
            } else {
                runInterpreter(args[0]);
            }
        } catch (final Exception e) {
            err.println(e.getMessage());
            e.printStackTrace(err);
            return 255;
        }

        return 0;
    }

    private void startRepl() throws IOException {
        final Console console = System.console();

        while (true) {
            final String data = console.readLine("~> ");

            if (data == null) {
                // EOF sent
                break;
            }

            final Object result = eval(parser.read(new ByteArrayInputStream(data.getBytes())));

            if (result != SlartiList.EMPTY) {
                out.println(result);
            }
        }
    }

    private void runInterpreter(String filename) throws IOException {
        eval(parser.read(new FileInputStream(filename)));
    }

    Object eval(Collection<SlartiNode> nodes) {
        Object result = SlartiList.EMPTY;

        for (final SlartiNode node : nodes) {
            result = node.eval(env);
        }

        return result;
    }

}
