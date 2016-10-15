package de.weltraumschaf.slartibartfass;

import com.beust.jcommander.Parameter;
import de.weltraumschaf.commons.jcommander.JCommanderImproved;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Command line options and help.
 *
 * @author Sven Strittmatter
 */
public final class CliOptions {
    /**
     * Tha file name of the wrapper script.
     */
    private static final String PROG_NAME = "slarti";
    /**
     * The usage string for help.
     */
    private static final String USAGE = PROG_NAME + " [-d|--debug] [-h|--help] [-v|--version] [<file1> <file2> ... <fileN>]";
    /**
     * Description of the tool for help.
     */
    private static final String DESCRIPTION =
        "A simple LISP interpreter. The syntax is like Scheme.\n\n" +
            "You can either start a REPL by invoking this command without any arguments\n" +
            "or you can pass as many files to interpret to the command.";
    /**
     * Example usages for help.
     */
    private static final String EXAMPLES =
        "$> slarti                   Start the REPL. Stop it with ctrl + c. Get some help with `!help` in the REPL.\n" +
            "  $> slarti hello_world.sl  Interpret the file hello_world.sl.";

    /**
     * Debug option.
     */
    @Parameter(names = {"-d", "--debug"}, description = "Enables debug output to STDOUT.")
    private boolean debug;
    /**
     * Show help option.
     */
    @Parameter(names = {"-h", "--help"}, description = "Show this help.", help = true)
    private boolean help;
    /**
     * Show version option.
     */
    @Parameter(names = {"-v", "--version"}, description = "Show version.")
    private boolean version;
    /**
     * The files to parse and interpret.
     */
    @Parameter(description = "Source files to parse. If omitted then a REPL is started.")
    private List<String> files = new ArrayList<>();

    /**
     * Whether to enable debug output.
     *
     * @return {@code true} for enable it, else {@code false}
     */
    public boolean isDebug() {
        return debug;
    }

    /**
     * Whether to show help.
     *
     * @return {@code true} to show it, else {@code false}
     */
    public boolean isHelp() {
        return help;
    }

    /**
     * Whether to show the version.
     *
     * @return {@code true} to show it, else {@code false}
     */
    public boolean isVersion() {
        return version;
    }

    /**
     * Get the list of source files to parse and interpret.
     *
     * @return never {@code null}, maybe empty, unmodifiable
     */
    public List<String> getFiles() {
        return Collections.unmodifiableList(files);
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof CliOptions)) {
            return false;
        }

        final CliOptions that = (CliOptions) o;
        return debug == that.debug &&
            help == that.help &&
            version == that.version &&
            Objects.equals(files, that.files);
    }

    @Override
    public int hashCode() {
        return Objects.hash(debug, help, version, files);
    }

    @Override
    public String toString() {
        return "CliOptions{"
            + "debug=" + debug
            + ", help=" + help
            + ", version=" + version
            + ", files=" + files
            + '}';
    }

    /**
     * Convenience method to create a CLI args parser.
     *
     * @return never {@code null}
     */
    static JCommanderImproved<CliOptions> newCliArgParser() {
        return new JCommanderImproved<>(CliOptions.PROG_NAME, CliOptions.class);
    }

    /**
     * Convenience method to generate the help message.
     *
     * @param cliArgs must not be {@code null}
     * @return never {@code null} or empty
     */
    String helpMessage(final JCommanderImproved<CliOptions> cliArgs) {
        return cliArgs.helpMessage(CliOptions.USAGE, CliOptions.DESCRIPTION, CliOptions.EXAMPLES);
    }
}
