package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.backend.Environment;

/**
 * Boolean type of the language.
 * <p>
 *     {@link #eval(Environment) Evaluating} this node will return its bare boolean representation.
 * </p>
 */
public final class SlartiBoolean extends ValueBasedType<Boolean> {
    /**
     * Represents the {@code #true} value.
     * <p>
     *     This object is shared because it is immutable.
     * </p>
     */
    public static final SlartiBoolean TRUE = new SlartiBoolean(Boolean.TRUE);
    /**
     * Represents the {@code #false} value.
     * <p>
     *     This object is shared because it is immutable.
     * </p>
     */
    public static final SlartiBoolean FALSE = new SlartiBoolean(Boolean.FALSE);
    /**
     * Literal token for {@code true}.
     */
    static final String TRUE_TOKEN = "#true";
    /**
     * Literal token for {@code false}.
     */
    static final String FALSE_TOKEN = "#false";

    /**
     * Use either {@link #TRUE} or {@link #FALSE}.
     *
     * @param value must not be {@code null}
     */
    private SlartiBoolean(final Boolean value) {
        super(value);
    }

    @Override
    public SlartiBoolean castToBoolean() {
        return this;
    }

    @Override
    public SlartiInteger castToInteger() {
        return Boolean.TRUE.equals(value()) ? new SlartiInteger(1L) : new SlartiInteger(0L);
    }

    @Override
    public SlartiReal castToReal() {
        return Boolean.TRUE.equals(value()) ? new SlartiReal(1d) : new SlartiReal(0d);
    }

    @Override
    public SlartiString castToString() {
        return new SlartiString(toString());
    }

    @Override
    public SlartiList castToList() {
        return new SlartiList(this);
    }

    @Override
    public String toString() {
        return value() ? TRUE_TOKEN : FALSE_TOKEN;
    }
}
