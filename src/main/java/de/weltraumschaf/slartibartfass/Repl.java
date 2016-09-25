package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.commons.application.Version;
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

/**
 * Provides a read eval print loop.
 */
final class Repl {
    private static final String WELCOME =
        " ____  _            _   _ _                _    __               \n" +
        "/ ___|| | __ _ _ __| |_(_) |__   __ _ _ __| |_ / _| __ _ ___ ___ \n" +
        "\\___ \\| |/ _` | '__| __| | '_ \\ / _` | '__| __| |_ / _` / __/ __|\n" +
        " ___) | | (_| | |  | |_| | |_) | (_| | |  | |_|  _| (_| \\__ \\__ \\\n" +
        "|____/|_|\\__,_|_|   \\__|_|_.__/ \\__,_|_|   \\__|_|  \\__,_|___/___/\n" +
        "                                                                 \n";
    public static final String PROMPT = "sl> ";
    private final IO io;
    private final SlartiVisitor<SlartiNode> visitor;
    private final Environment env;
    private final boolean isDebugEnabled;

    /**
     * Dedicated constructor.
     *  @param io must not be {@code null}
     * @param visitor must not be {@code null}
     * @param env must not be {@code null}
     * @param isDebugEnabled whether to print debug output
     */
    Repl(final IO io, final SlartiVisitor<SlartiNode> visitor, final Environment env, boolean isDebugEnabled) {
        super();
        this.io = Validate.notNull(io, "io");
        this.visitor = Validate.notNull(visitor, "visitor");
        this.env = Validate.notNull(env, "env");
        this.isDebugEnabled = isDebugEnabled;
    }

    /**
     * Starts the REPL.
     * <p>
     *     The REPL ends if {@code null} is read as input.
     * </p>
     *
     * @param version must not be {@code null}
     * @throws IOException if the REPL can't read from the console
     */
    void start(final Version version) throws IOException {
        final Parsers parsers = new Parsers(io);
        final ConsoleReader reader = createReader();
        welcome(version);

        while (true) {
            final String data = reader.readLine();

            if (data == null) {
                break; // EOF sent
            }

            if (Command.isCmd(data)) {
                try {
                    execute(Command.getCmd(data));
                } catch (final ExitRepl e) {
                    io.println(PROMPT + "Bye bye :-)");
                    break;
                }

                continue;
            }

            try {
                final SlartiParser parser = parsers.newParser(new ByteArrayInputStream(data.getBytes()), isDebugEnabled);
                final SlartiNode node = visitor.visit(parser.file());
                final Object result = node.eval(env);

                if (InternalList.EMPTY.equals(result)) {
                    // Do not print empty list results.
                    continue;
                }

                io.println(result.toString());
            } catch (final SlartiError e) {
                io.errorln("[E] " + e.getMessage());

                if (isDebugEnabled) {
                    printStackTraceOnDebug(e);
                }

            } catch (RuntimeException e) {
                io.errorln("[F] " + e.getMessage());

                if (isDebugEnabled) {
                    printStackTraceOnDebug(e);
                }
            }
        }
    }

    private void welcome(Version version) {
        io.print(WELCOME);
        io.println(String.format("Welcome to Slartibartfass REPL v%s.", version.getVersion()));
        io.println("");
        io.println(String.format("  Type %s for help.", Command.HELP));
        io.println("");
    }

    private void printStackTraceOnDebug(final Throwable e) {
        e.printStackTrace(io.getStderr());
    }

    private void execute(final Command cmd) {
        switch (cmd) {
            case ENV:
                env.print(io.getStdout());
                break;
            case EXIT:
                throw new ExitRepl();
            case HELP:
                Command.printHelp(io.getStdout());
                break;
            default:
                io.errorln("[E] Unknown command: '" + cmd + "'!");
                break;
        }
    }

    private ConsoleReader createReader() throws IOException {
        System.setProperty("jline.expandevents", Boolean.FALSE.toString());
        final ConsoleReader reader = new ConsoleReader(io.getStdin(), io.getStdout());
        reader.setBellEnabled(false);
        reader.addCompleter(createCompletionHints());
        reader.setPrompt(PROMPT);
        return reader;
    }

    private Completer createCompletionHints() {
        return new CommandEnumCompleter(Command.class);
    }

    /**
     * Special commands in the REPL.
     * <p>
     *     These commands are not part of the language itself.
     * </p>
     */
    private enum Command {
        ENV("Shows the environment from the current scope up to the root."),
        EXIT("Stops the REPL and exits."),
        HELP("Shows this help.");

        /**
         * Escape the command to distinguish them from usual syntax.
         */
        private static final char PREFIX = '!';
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

    /**
     * Provides tab completion for the REPL commands to the console reader.
     */
    private static final class CommandEnumCompleter extends StringsCompleter {
        CommandEnumCompleter(Class<? extends Enum> source) {
            checkNotNull(source);

            for (Enum<?> n : source.getEnumConstants()) {
                this.getStrings().add(n.toString().toLowerCase());
            }
        }
    }

    private static final class ExitRepl extends RuntimeException {

    }
}
