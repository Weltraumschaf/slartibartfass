package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.commons.application.Version;
import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.frontend.SlartiParser;
import de.weltraumschaf.slartibartfass.frontend.SlartiVisitor;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
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
 * <p>
 *     The REPL reads a user input line until newline, then parse and interpret it and finally prints the result
 *     back to the user. It upports the same syntax like the interpreted files. Aso the REPL supports some iternal
 *     {@link Command commands} to do things not provided by the language itself: E.g. show the allocated memory
 *     in the  environment.
 * </p>
 *
 * @author Sven Strittmatter
 */
final class Repl {
    /**
     * Greeting to the user.
     */
    private static final String WELCOME =
        " ____  _            _   _ _                _    __               \n" +
        "/ ___|| | __ _ _ __| |_(_) |__   __ _ _ __| |_ / _| __ _ ___ ___ \n" +
        "\\___ \\| |/ _` | '__| __| | '_ \\ / _` | '__| __| |_ / _` / __/ __|\n" +
        " ___) | | (_| | |  | |_| | |_) | (_| | |  | |_|  _| (_| \\__ \\__ \\\n" +
        "|____/|_|\\__,_|_|   \\__|_|_.__/ \\__,_|_|   \\__|_|  \\__,_|___/___/\n" +
        "                                                                 \n";
    private static final String INITIAL_HELP = "Hello, World example:\n" +
        "(println \"Hello, World!\")\n";
    /**
     * The REPL prompt to signal that user input is expected.
     */
    private static final String PROMPT = "sl> ";
    /**
     * Injected I/O streams
     */
    private final IO io;
    /**
     * Visitor to convert the parsed input.
     */
    private final SlartiVisitor<SlartiNode> visitor;
    /**
     * The root scope to allocate memory.
     */
    private final Environment env;
    /**
     * Whether to print debug information.
     */
    private final boolean isDebugEnabled;
    /**
     * Flag to signal that the loop should be exited.
     */
    private boolean exit;

    /**
     * Dedicated constructor.
     *
     * @param io must not be {@code null}
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
                execute(Command.getCmd(data));

                if (exit) {
                    io.println(PROMPT + "Bye bye :-)");
                    break;
                }

                continue;
            }


            try {
                final SlartiParser parser = parsers.newParser(new ByteArrayInputStream(data.getBytes()), isDebugEnabled);
                final SlartiNode node = visitor.visit(parser.file());
                final Object result = node.eval(env);

                if (SlartiList.NIL.equals(result)) {
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

    /**
     * Show a welcome message to the user.
     *
     * @param version must not be {@code null}
     */
    private void welcome(final Version version) {
        io.print(WELCOME);
        io.println(String.format("Welcome to Slartibartfass REPL v%s.", version.getVersion()));
        io.println("");
        io.println(INITIAL_HELP);
        io.println("");
        io.println(String.format("  Type %s for help.", Command.HELP));
        io.println("");
    }

    /**
     * Print the exception stack trace to the error output stream.
     *
     * @param e must not be {@code null}
     */
    private void printStackTraceOnDebug(final Throwable e) {
        e.printStackTrace(io.getStderr());
    }

    /**
     * Executes a REPL command.
     *
     * @param cmd if {@code null} a error message will be printed to the user.
     */
    private void execute(final Command cmd) {
        switch (cmd) {
            case ENV:
                env.print(io.getStdout());
                break;
            case EXIT:
                exit = true;
                break;
            case HELP:
                Command.printHelp(io.getStdout());
                break;
            default:
                io.errorln("[E] Unknown command: '" + cmd + "'!");
                break;
        }
    }

    /**
     * Facotry method to create the interactive console.
     *
     * @return never {@code null}, always new instance
     * @throws IOException if the I/O streams can't be written/read
     */
    private ConsoleReader createReader() throws IOException {
        // Disable this so we can use the bang (!) for our commands as prefix.
        System.setProperty("jline.expandevents", Boolean.FALSE.toString());
        final ConsoleReader reader = new ConsoleReader(io.getStdin(), io.getStdout());
        reader.setBellEnabled(false);
        reader.addCompleter(createCompletionHints());
        reader.setPrompt(PROMPT);
        return reader;
    }

    /**
     * Create completion hints for the interactive console.
     *
     * @return never {@code null}, always new instance
     */
    private Completer createCompletionHints() {
        return new CommandEnumCompleter(Command.class);
    }

    /**
     * Special commands in the REPL.
     * <p>
     *     These commands are not part of the language itself and are treated case sensitive.
     * </p>
     */
    private enum Command {
        /**
         * Shows the allocated  memory.
         */
        ENV("Shows the environment from the current scope up to the root."),
        /**
         * Exits the REPL.
         */
        EXIT("Stops the REPL and exits."),
        /**
         * Shows help about the REPL commands.
         */
        HELP("Shows this help.");

        /**
         * Escape the command to distinguish them from usual syntax.
         */
        private static final char PREFIX = '!';
        /**
         * Lookup table to find the command enum by symbol.
         */
        private static final Map<String, Command> LOOKUP = new HashMap<>();
        static {
            Arrays.stream(Command.values()).forEach(c -> LOOKUP.put(c.toString(), c));
        }

        /**
         * Help message of the command.
         */
        private final String help;

        /**
         * Dedicated constructor.
         *
         * @param help must not be {@code null} or empty
         */
        Command(final String help) {
            this.help = Validate.notEmpty(help, "help");
        }

        @Override
        public String toString() {
            return PREFIX + name().toLowerCase();
        }

        /**
         * Whether a given string is a command.
         *
         * @param in the tested string
         * @return {@code true} if it is a command, else {@code false}
         */
        public static boolean isCmd(final String in) {
            if (null == in) {
                return false;
            }

            return LOOKUP.containsKey(in.trim());
        }


        /**
         * Get the command enum for given string.
         * <p>
         *     Use {@link #isCmd(String)} to check if there is a command for the givne string.
         * </p>
         *
         * @param in must not be {@code null}
         * @return never {@code null}
         */
        public static Command getCmd(final String in) {
            return LOOKUP.get(Validate.notNull(in, "in").trim());
        }

        /**
         * Print he help for all commands to the givne output stream.
         *
         * @param out must not be {@code null}
         */
        public static void printHelp(final PrintStream out) {
            Validate.notNull(out, "out");
            out.println("Available commands:");
            Arrays.stream(values()).forEach( c -> out.println(String.format("  %1$-8s", c.toString()) + c.help));
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

}
