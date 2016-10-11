package de.weltraumschaf.slartibartfass;

/**
 * Builder to format console output with <a href="https://en.wikipedia.org/wiki/ANSI_escape_code">ANSI escape codes</a>.
 *
 * @author Sven Strittmatter
 */
public final class Ansi {

    /**
     * Escape sequence format.
     */
    private static final String ESCAPE_SEQUENCE = "\u001B[%dm";
    /**
     * Buffers the formatted string.
     */
    private final StringBuilder buffer = new StringBuilder();

    /**
     * Use {@link #fmt()}}.
     */
    private Ansi() {
        super();
    }

    /**
     * Factory method to fmt new builder.
     *
     * @return never {@code null}, always new instance
     */
    public static Ansi fmt() {
        return new Ansi();
    }

    /**
     * Reset all previous formatting.
     *
     * @return self for chaining
     */
    public Ansi reset() {
        buffer.append(ansi(0));
        return this;
    }

    /**
     * Start bold formatted text.
     *
     * @return self for chaining
     */
    public Ansi bold() {
        buffer.append(ansi(1));
        return this;
    }

    /**
     * Start italic formatted text.
     *
     * @return self for chaining
     */
    public Ansi italic() {
        buffer.append(ansi(3));
        return this;
    }

    /**
     * Start foreground color text.
     *
     * @return self for chaining
     */
    public Ansi fg(final Color color) {
        buffer.append(ansi(color.value + 30));
        return this;
    }

    /**
     * Start bright foreground color text.
     *
     * @return self for chaining
     */
    public Ansi fgBright(final Color color) {
        buffer.append(ansi(color.value + 90));
        return this;
    }

    /**
     * Start background color text.
     *
     * @return self for chaining
     */
    public Ansi bg(final Color color) {
        buffer.append(ansi(color.value + 40));
        return this;
    }

    /**
     * Start bright background color text.
     *
     * @return self for chaining
     */
    public Ansi bgBright(final Color color) {
        buffer.append(ansi(color.value + 100));
        return this;
    }

    /**
     * Add formatted text to buffer.
     *
     * @param text must not be {@code null}
     * @param args optional format arguments
     * @return self for chaining
     */
    public Ansi text(final String text, final Object... args) {
        buffer.append(String.format(text, args));
        return this;
    }

    /**
     * Add a new line to the buffer.
     *
     * @return self for chaining
     */
    public Ansi nl() {
        buffer.append('\n');
        return this;
    }

    private String ansi(final int code) {
        return String.format(ESCAPE_SEQUENCE, code);
    }

    @Override
    public String toString() {
        return buffer.toString();
    }

    /**
     * Available color codes.
     */
    public enum Color {
        /**
         * Escape sequence for black colored text.
         */
        BLACK(0),
        /**
         * Escape sequence for red colored text.
         */
        RED(1),
        /**
         * Escape sequence for green colored text.
         */
        GREEN(2),
        /**
         * Escape sequence for yellow colored text.
         */
        YELLOW(3),
        /**
         * Escape sequence for blue colored text.
         */
        BLUE(4),
        /**
         * Escape sequence for magenta colored text.
         */
        MAGENTA(5),
        /**
         * Escape sequence for cyan colored text.
         */
        CYAN(6),
        /**
         * Escape sequence for white colored text.
         */
        WHITE(7),
        /**
         * Escape sequence for default colored text.
         */
        DEFAULT(9);

        private final int value;

        Color(final int value) {
            this.value = value;
        }
    }
}
