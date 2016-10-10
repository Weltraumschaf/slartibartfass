package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.validate.Validate;

/**
 * Helper class to format console output with escape sequences.
 * <p>
 * For a list of escape sequences see <a href="https://en.wikipedia.org/wiki/ANSI_escape_code">Wikipedia</a></a>
 * </p>
 *
 * @author Sven Strittmatter
 */
public final class ConsoleFormatter {
    /**
     * Escape character which starts all sequences.
     */
    private static final char ESCAPE = '\u001B';
    /**
     * Clear all styles from previous escape sequences.
     */
    private static final String RESET = ESCAPE + "[0m";
    /**
     * Escape sequence to start bold font style.
     */
    private static final String BOLD = ESCAPE + "[1m";
    /**
     * Escape sequence to start italic font style.
     */
    private static final String ITALIC = ESCAPE + "[3m";

    /**
     * Wraps given input with escape sequences to format the input text bold at console output.
     *
     * @param input must not be {@code null}
     * @return never {@code null} or empty
     */
    public String bold(final String input) {
        return BOLD + Validate.notNull(input, "input") + RESET;
    }

    /**
     * Wraps given input with escape sequences to format the input text italic at console output.
     *
     * @param input must not be {@code null}
     * @return never {@code null} or empty
     */
    public String italic(final String input) {
        return ITALIC + Validate.notNull(input, "input") + RESET;
    }

    /**
     * Wraps given input with escape sequences to format the input text colored at console output.
     *
     * @param color must not be {@code null}
     * @param input must not be {@code null}
     * @return never {@code null} or empty
     */
    public String color(final Color color, final String input) {
        return Validate.notNull(color, "color").escapeSequence() + Validate.notNull(input, "input") + RESET;
    }

    /**
     * Available color codes.
     */
    public enum Color {
        /**
         * Escape sequence for black colored font.
         */
        BLACK(30),
        /**
         * Escape sequence for red colored font.
         */
        RED(31),
        /**
         * Escape sequence for green colored font.
         */
        GREEN(32),
        /**
         * Escape sequence for yellow colored font.
         */
        YELLOW(33),
        /**
         * Escape sequence for blue colored font.
         */
        BLUE(34),
        /**
         * Escape sequence for magenta colored font.
         */
        MAGENTA(35),
        /**
         * Escape sequence for cyan colored font.
         */
        CYAN(36),
        /**
         * Escape sequence for white colored font.
         */
        WHITE(37);

        /**
         * The literal color code.
         */
        private final int code;

        /**
         * Dedicated constructor.
         *
         * @param code from 30 to 37
         */
        Color(final int code) {
            this.code = code;
        }

        /**
         * Creates the whole escape sequence string for the color.
         *
         * @return never {@code null} or empty
         */
        private String escapeSequence() {
            return "" + ESCAPE + '[' + code + 'm';
        }
    }
}
