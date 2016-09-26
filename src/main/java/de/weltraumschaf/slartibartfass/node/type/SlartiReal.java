package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

import java.util.Objects;

/**
 * Real type of the language.
 * <p>
 *     {@link #eval(Environment) Evaluating} this node will return its bare double representation.
 * </p>
 *
 * @author Sven Strittmatter
 */
public final class SlartiReal extends ValueBasedType<Double> {

    /**
     * The bare zero used for casting.
     */
    private static final Double ZERO_VALUE = 0d;
    /**
     * Reused constant to save memory.
     */
    static final SlartiReal ZERO = new SlartiReal(ZERO_VALUE);

    /**
     * Dedicated constructor.
     *
     * @param value must not be {@code null}
     */
    public SlartiReal(final Double value) {
        super(value);
    }

    @Override
    public SlartiBoolean castToBoolean() {
        return ZERO_VALUE.equals(value()) ? SlartiBoolean.FALSE : SlartiBoolean.TRUE;
    }

    @Override
    public SlartiInteger castToInteger() {
        return new SlartiInteger(value().longValue());
    }

    @Override
    public SlartiReal castToReal() {
        return this;
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
