package de.weltraumschaf.slartibartfass;

/**
 * Enumerates constant values.
 *
 * @author Sven Strittmatter
 */
public enum Constants {
    /**
     * The base package of the whole module.
     */
    BASE_PACKAGE("/de/weltraumschaf/slartibartfass");

    /**
     * The literal value.
     */
    private final String value;

    /**
     * Dedicated constructor.
     *
     * @param value must not b {@code null}
     */
    Constants(final String value) {
        this.value = value;
    }

    /**
     * Get the constant value.
     *
     * @return never {@code null}
     */
    public String value() {
        return value;
    }
}
