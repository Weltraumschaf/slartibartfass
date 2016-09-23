package de.weltraumschaf.slartibartfass;

/**
 * USed for any runtime error.
 */
public class SlartiError extends RuntimeException {
    public SlartiError(final String message, final Object ... args) {
        this(String.format(message, args));
    }

    public SlartiError(final String message) {
        super(message);
    }
}
