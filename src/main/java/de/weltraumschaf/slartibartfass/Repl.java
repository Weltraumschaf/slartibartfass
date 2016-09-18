package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.frontend.SlartiParser;
import de.weltraumschaf.slartibartfass.frontend.SlartiVisitor;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import jline.console.ConsoleReader;
import jline.console.completer.Completer;
import jline.console.completer.StringsCompleter;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static jline.internal.Preconditions.checkNotNull;

public final class Repl {
    private final IO io;
    private final SlartiVisitor<SlartiNode> visitor;
    private final Environment env;

    public Repl(final IO io, final SlartiVisitor<SlartiNode> visitor, final Environment env) {
        super();
        this.io = Validate.notNull(io, "io");
        this.visitor = Validate.notNull(visitor, "visitor");
        this.env = Validate.notNull(env, "env");
    }

    public void start(final boolean isDebugEnabled) throws IOException {
        final Parsers parsers = new Parsers(io);
        final ConsoleReader reader = createReader();

        while (true) {
            final String data = reader.readLine();

            if (data == null) {
                break; // EOF sent
            }

            if (Command.isCmd(data)) {
                execute(Command.getCmd(data));
                continue;
            }

            try {
                final SlartiParser parser = parsers.newParser(new ByteArrayInputStream(data.getBytes()), isDebugEnabled);
                final Object result = visitor.visit(parser.file()).eval(env);
                io.println(result.toString());
            } catch (final SlartiError e) {
                io.errorln("[E] " + e.getMessage());
            }
        }
    }

    private void execute(final Command cmd) {
        switch (cmd) {
            case ENV:
                env.print(io.getStdout());
                break;
            case HELP:
                Command.printHelp(io.getStdout());
                break;
            default:
                io.errorln("[E] Unknown command: '" + cmd + "'!");
                break;
        }
    }

    private ConsoleReader createReader() throws IOException {
        final ConsoleReader reader = new ConsoleReader(io.getStdin(), io.getStdout());
        reader.setBellEnabled(false);
        reader.addCompleter(createCompletionHints());
        reader.setPrompt("sl> ");
        return reader;
    }

    private Completer createCompletionHints() {
        return new CommandEnumCompleter(Command.class);
    }

    private enum Command {
        ENV("Shows the environment from the current scope up to the root."),
        HELP("Shows this help.");

        private static final char PREFIX = '$';
        private static final Map<String, Command> LOOKUP = new HashMap<>();
        static {
            Arrays.stream(Command.values()).forEach(c -> LOOKUP.put(c.toString(), c));
        }

        private final String help;

        Command(final String help) {
            this.help = help;
        }

        @Override
        public String toString() {
            return PREFIX + name().toLowerCase();
        }

        public static boolean isCmd(final String in) {
            return LOOKUP.containsKey(in.trim());
        }

        public static Command getCmd(final String in) {
            return LOOKUP.get(in.trim());
        }

        public static void printHelp(final PrintStream stdout) {
            stdout.println("Available commands:");
            Arrays.stream(values()).forEach( c -> stdout.println(String.format("  %1$-8s", c.toString()) + c.help));
        }
    }

    private static final class CommandEnumCompleter extends StringsCompleter {
        CommandEnumCompleter(Class<? extends Enum> source) {
            checkNotNull(source);

            for (Enum<?> n : source.getEnumConstants()) {
                this.getStrings().add(n.toString().toLowerCase());
            }
        }
    }
}
