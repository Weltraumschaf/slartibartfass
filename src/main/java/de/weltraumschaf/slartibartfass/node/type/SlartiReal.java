package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.SlartiType;

import java.util.Objects;

/**
 * Real type of the language.
 * <p>
 *     {@link #eval(Environment) Evaluating} this node will return its bare double representation.
 * </p>
 */
public final class SlartiReal implements SlartiNode, SlartiType<Double> {
    @SuppressWarnings("UnnecessaryBoxing")
    private static final Double ZERO_VALUE = Double.valueOf(0d);
    static final SlartiReal ZERO = new SlartiReal(ZERO_VALUE);
    private final Double value;

    /**
     * Dedicated constructor.
     *
     * @param value must not be {@code null}
     */
    public SlartiReal(final Double value) {
        super();
        this.value = Validate.notNull(value, "value");
    }

    @Override
    public Object eval(final Environment env) {
        return value;
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof SlartiReal)) {
            return false;
        }

        final SlartiReal that = (SlartiReal) o;
        return Objects.equals(value, that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public String toString() {
        return "" + value;
    }

    @Override
    public Double value() {
        return value;
    }

    @Override
    public SlartiBoolean castToBoolean() {
        return ZERO_VALUE.equals(value) ? SlartiBoolean.FALSE : SlartiBoolean.TRUE;
    }

    @Override
    public SlartiInteger castToInteger() {
        return new SlartiInteger(value.longValue());
    }

    @Override
    public SlartiReal castToReal() {
        return this;
    }

    @Override
    public SlartiString castToString() {
        return new SlartiString(toString());
    }
}
