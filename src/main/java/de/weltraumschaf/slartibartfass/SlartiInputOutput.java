package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.commons.validate.Validate;

import java.io.InputStream;
import java.io.PrintStream;

/**
 * This class performs formatted printing to output streams.
 * <p>
 * This class wraps an {@link IO} to add some {@link Ansi ANSI formatting}.
 * </p>
 *
 * @author Sven Strittmatter
 */
public final class SlartiInputOutput {
    /**
     * Used to delegate the I/O.
     */
    private final IO io;
    /**
     * whether to print debug lines or not.
     */
    private final boolean debugEnabled;

    /**
     * Convenience constructor with disabled debugging.
     *
     * @param io must not be {@code null}
     */
    public SlartiInputOutput(final IO io) {
        this(io, false);
    }

    /**
     * Dedicated constructor.
     *
     * @param io           must not be {@code null}
     * @param debugEnabled whether to print debug lines or not
     */
    public SlartiInputOutput(final IO io, final boolean debugEnabled) {
        super();
        this.io = Validate.notNull(io, "io");
        this.debugEnabled = debugEnabled;
    }

    /**
     * Prints a formatted debug line.
     *
     * @param messageFormat must not be {@code null} or empty
     * @param args          optional format arguments
     */
    public void debug(final String messageFormat, final Object... args) {
        if (debugEnabled) {
            io.println(Ansi.fmt().fg(Ansi.Color.BLUE).text("[D] ").text(messageFormat, args).reset().toString());
        }
    }

    /**
     * Prints a formatted debug line.
     *
     * @param messageFormat must not be {@code null} or empty
     * @param args          optional format arguments
     */
    public void error(final String messageFormat, final Object... args) {
        io.errorln(Ansi.fmt().fg(Ansi.Color.RED).text("[E] ").text(messageFormat, args).reset().toString());
    }

    /**
     * Prints a formatted debug line.
     *
     * @param messageFormat must not be {@code null} or empty
     * @param args          optional format arguments
     */
    public void fatal(final String messageFormat, final Object... args) {
        io.errorln(Ansi.fmt().fg(Ansi.Color.RED).text("[F] ").text(messageFormat, args).reset().toString());
    }

    /**
     * Print the exception stack trace to the error output stream.
     * <p>
     * It only prints the stack trace if {@link #debugEnabled debug is enabled}.
     * </p>
     *
     * @param e must not be {@code null}
     */
    public void printStackTraceOnDebug(final Throwable e) {
        if (debugEnabled) {
            io.printStackTrace(e);
        }
    }

    /**
     * Delegate method.
     *
     * @param str must not be {@code null}
     * @see IO#print(String)
     */
    public void print(final String str) {
        io.print(str);
    }

    /**
     * Delegate method.
     *
     * @param str must not be {@code null}
     * @see IO#println(String)
     */
    public void println(final String str) {
        io.println(str);
    }

    /**
     * Delegate method.
     *
     * @see IO#getStdout()
     */
    public PrintStream getStdout() {
        return io.getStdout();
    }

    /**
     * Delegate method.
     *
     * @see IO#getStdin()
     */
    public InputStream getStdin() {
        return io.getStdin();
    }

    /**
     * Get the wrapped {@link IO}.
     *
     * @return never {@code null}
     */
    public IO getIo() {
        return io;
    }
}
