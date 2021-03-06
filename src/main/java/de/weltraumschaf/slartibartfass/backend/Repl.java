package de.weltraumschaf.slartibartfass.backend;

import de.weltraumschaf.commons.application.Version;
import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.*;
import de.weltraumschaf.slartibartfass.frontend.SlartiParser;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import jline.console.ConsoleReader;
import jline.console.completer.Completer;
import jline.console.completer.StringsCompleter;

import java.io.*;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import static jline.internal.Preconditions.checkNotNull;

/**
 * Provides a read eval print loop.
 * <p>
 * The REPL reads a user input line until newline, then parse and interpret it and finally prints the result
 * back to the user. It supports the same syntax like the interpreted files. Aso the REPL supports some internal
 * {@link Command commands} to do things not provided by the language itself: E.g. show the allocated memory
 * in the  environment.
 * </p>
 *
 * @author Sven Strittmatter
 */
final class Repl extends BaseExecutor {
    /**
     * Greeting to the user.
     */
    private static final String WELCOME =
        "           Welcome to Slartibartfass REPL v%s            \n";
    /**
     * Some initial help examples for beginners.
     */
    private static final String INITIAL_HELP = "Hello, World example:\n"
        + "  (println \"Hello, World!\")\n\n"
        + "A simple math expression:\n"
        + "  (* 3 (+ 2 3 4))\n\n"
        + "Define a variable and print it:\n"
        + "  (define foo \"my var\")\n"
        + "  (println foo)\n\n"
        + "Define a function and call it:\n"
        + "  (define (bar x) (* 3 x))\n"
        + "  (bar 23)\n";
    /**
     * The REPL prompt to signal that user input is expected.
     */
    private static final String PROMPT = "sl> ";
    /**
     * Used to print version info.
     */
    private final Version version;
    /**
     * Flag to signal that the loop should be exited.
     */
    private boolean exit;

    /**
     * Dedicated constructor.
     *
     * @param io      must not be {@code null}
     * @param version must not be {@code null}
     */
    Repl(final SlartiInputOutput io, final Version version) {
        super(io);
        this.version = Validate.notNull(version, "version");
    }

    @Override
    public void start() throws IOException {
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
                    io().println(Ansi.fmt().fg(Ansi.Color.BLUE).text("Bye bye :-)").reset().toString());
                    break;
                }

                continue;
            }

            try {
                final SlartiParser parser = parsers().newParser(new ByteArrayInputStream(data.getBytes()));
                final SlartiNode node = visitor().visit(parser.file());
                final Object result = node.eval(env());

                if (SlartiList.NIL.equals(result)) {
                    // Do not print empty list results.
                    continue;
                }

                io().println(Ansi.fmt().fg(Ansi.Color.GREEN).bold().text(result.toString()).reset().toString());
            } catch (final SlartiError e) {
                io().error(e.getMessage());
                io().printStackTraceOnDebug(e);
            } catch (RuntimeException e) {
                io().fatal(e.getMessage());
                io().printStackTraceOnDebug(e);
            }
        }
    }

    /**
     * Show a welcome message to the user.
     *
     * @param version must not be {@code null}
     * @throws IOException if figlet can't be read
     */
    private void welcome(final Version version) throws IOException {
        io().print(Ansi.fmt()
            .fg(Ansi.Color.BLUE).bold().text(figlet()).reset()
            .nl().nl()
            .fg(Ansi.Color.BLUE).italic().text(WELCOME, version).reset()
            .nl()
            .bold().text("  Type %s for help.", Command.HELP).reset()
            .nl().nl()
            .toString());
    }

    /**
     * Reads the figlet from file.
     *
     * @return never {@code null}
     * @throws IOException if figlet can't be read
     */
    private String figlet() throws IOException {
        final InputStream input = getClass().getResourceAsStream(Constants.BASE_PACKAGE.value() + "/figlet");

        try (BufferedReader buffer = new BufferedReader(new InputStreamReader(input))) {
            return buffer.lines().collect(Collectors.joining("\n"));
        }
    }

    /**
     * Executes a REPL command.
     *
     * @param cmd if {@code null} a error message will be printed to the user.
     */
    private void execute(final Command cmd) {
        switch (cmd) {
            case CLEAR:
                env(new Environment());
                init();
                io().println("Environment cleared.");
                break;
            case ENV:
                env().print(io().getStdout());
                break;
            case EXAMPLES:
                io().println(INITIAL_HELP);
                break;
            case EXIT:
                exit = true;
                break;
            case HELP:
                Command.printHelp(io().getStdout());
                break;
            default:
                io().error("Unknown command: '" + cmd + "'!");
                break;
        }
    }

    /**
     * Factory method to fmt the interactive console.
     *
     * @return never {@code null}, always new instance
     * @throws IOException if the I/O streams can't be written/read
     */
    private ConsoleReader createReader() throws IOException {
        // Disable this so we can use the bang (!) for our commands as prefix.
        System.setProperty("jline.expandevents", Boolean.FALSE.toString());
        final ConsoleReader reader = new ConsoleReader(io().getStdin(), io().getStdout());
        reader.setBellEnabled(false);
        reader.addCompleter(createCompletionHints());
        reader.setPrompt(Ansi.fmt().bold().fg(Ansi.Color.BLUE).text(PROMPT).reset().toString());
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
     * These commands are not part of the language itself and are treated case sensitive.
     * </p>
     */
    private enum Command {
        /**
         * Clears the user defined symbols.
         */
        CLEAR("Clears the whole environment: Removes all defined symbols, but reloads built in and STD lib."),
        /**
         * Shows the allocated  memory.
         */
        ENV("Shows the environment from the current scope up to the root."),
        /**
         * Show some examples.
         */
        EXAMPLES("Show some example code snippets."),
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
            return null != in && LOOKUP.containsKey(in.trim());

        }

        /**
         * Get the command enum for given string.
         * <p>
         * Use {@link #isCmd(String)} to check if there is a command for the given string.
         * </p>
         *
         * @param in must not be {@code null}
         * @return never {@code null}
         */
        public static Command getCmd(final String in) {
            return LOOKUP.get(Validate.notNull(in, "in").trim());
        }

        /**
         * Print he help for all commands to the given io stream.
         *
         * @param out must not be {@code null}
         */
        public static void printHelp(final PrintStream out) {
            Validate.notNull(out, "out");
            out.println("Available commands:");
            Arrays.stream(values()).forEach(c -> out.println(String.format("  %1$-10s", c.toString()) + c.help));
        }
    }

    /**
     * Provides tab completion for the REPL commands to the console reader.
     */
    private static final class CommandEnumCompleter extends StringsCompleter {
        /**
         * Dedicated constructor.
         *
         * @param source must not be {@code null}
         */
        CommandEnumCompleter(Class<? extends Enum> source) {
            checkNotNull(source);

            for (Enum<?> n : source.getEnumConstants()) {
                getStrings().add(n.toString().toLowerCase());
            }
        }
    }

}
