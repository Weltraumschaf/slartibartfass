package de.weltraumschaf.slartibartfass;

import com.beust.jcommander.Parameter;
import de.weltraumschaf.commons.jcommander.JCommanderImproved;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Command line options and help.
 *
 * @author Sven Strittmatter
 */
public final class CliOptions {
    private static final String PROG_NAME = "slarti";
    private static final String USAGE = PROG_NAME + " [-d|--debug] [-h|--help] [-v|--version] [<file1> <file2> ... <fileN>]";
    private static final String DESCRIPTION =
        "A simple LISP interpreter. The syntax is like Scheme.\n\n" +
        "You can either start a REPL by invoking this command without any arguments\n" +
        "or you can pass as many files to interpret to the command.";
    private static final String EXAMPLES =
        "$> slarti                   Start the REPL. Stop it with ctrl + c. Get some help with `!help` in the REPL.\n" +
        "  $> slarti hello_world.sl  Interpret the file hello_world.sl.";

    @Parameter(names = {"-d", "--debug"}, description = "Enables debug output to STDOUT.")
    private boolean debug;
    @Parameter(names = {"-h", "--help"}, description = "Show this help.", help = true)
    private boolean help;
    @Parameter(names = {"-v", "--version"}, description = "Show version.")
    private boolean version;
    @Parameter(description = "Source files to parse. If omitted then a REPL is started.")
    private List<String> files = new ArrayList<>();

    public boolean isDebug() {
        return debug;
    }

    public boolean isHelp() {
        return help;
    }

    public boolean isVersion() {
        return version;
    }

    public List<String> getFiles() {
        return files;
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
        return "CliOptions{" +
            "debug=" + debug +
            ", help=" + help +
            ", version=" + version +
            ", files=" + files +
            '}';
    }

    static JCommanderImproved<CliOptions> newCliArgParser() {
        return new JCommanderImproved<>(CliOptions.PROG_NAME, CliOptions.class);
    }

    String helpMessage(final JCommanderImproved<CliOptions> cliArgs) {
        return cliArgs.helpMessage(CliOptions.USAGE, CliOptions.DESCRIPTION, CliOptions.EXAMPLES);
    }
}
