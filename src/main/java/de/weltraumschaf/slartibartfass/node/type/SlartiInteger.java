package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.backend.Environment;

/**
 * Integer type of the language.
 * <p>
 * {@link #eval(Environment) Evaluating} this node will return its bare long representation.
 * </p>
 *
 * @author Sven Strittmatter
 */
public final class SlartiInteger extends ValueBasedType<Long> {

    /**
     * The bare zero used for casting.
     */
    private static final Long ZERO_VALUE = 0L;
    /**
     * Reused constant to save memory.
     */
    static final SlartiInteger ZERO = new SlartiInteger(ZERO_VALUE);

    /**
     * Dedicated constructor.
     *
     * @param value must not be {@code null}
     */
    public SlartiInteger(final Long value) {
        super(value);
    }

    @Override
    public SlartiBoolean castToBoolean() {
        return ZERO_VALUE.equals(value()) ? SlartiBoolean.FALSE : SlartiBoolean.TRUE;
    }

    @Override
    public SlartiInteger castToInteger() {
        return this;
    }

    @Override
    public SlartiReal castToReal() {
        return new SlartiReal(value().doubleValue());
    }

    @Override
    public SlartiString castToString() {
        return new SlartiString(toString());
    }

    @Override
    public SlartiList castToList() {
        return new SlartiList(this);
    }
}
