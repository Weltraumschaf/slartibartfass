package de.weltraumschaf.slartibartfass;

/**
 * USed for any runtime error.
 */
public class SlartiError extends RuntimeException {
    public SlartiError(String message) {
        super(message);
    }
}
