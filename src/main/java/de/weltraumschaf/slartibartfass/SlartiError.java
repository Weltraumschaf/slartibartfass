package de.weltraumschaf.slartibartfass;

/**
 * Used for any runtime error.
 *
 * @author Sven Strittmatter
 */
public class SlartiError extends RuntimeException {
    /**
     * The message is interpreted as format string.
     *
     * @param message format string
     * @param args    optional format arguments
     */
    public SlartiError(final String message, final Object... args) {
        super(String.format(message, args));
    }

}
