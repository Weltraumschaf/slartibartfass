package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.SlartiType;

import java.util.Objects;

/**
 * Integer type of the language.
 * <p>
 *     {@link #eval(Environment) Evaluating} this node will return its bare long representation.
 * </p>
 */
public final class SlartiInteger implements SlartiNode<Long> {
    @SuppressWarnings("UnnecessaryBoxing")
    private static final Long ZERO_VALUE = Long.valueOf(0L);
    static final SlartiInteger ZERO = new SlartiInteger(ZERO_VALUE);
    private final Long value;

    /**
     * Dedicated constructor.
     *
     * @param value must not be {@code null}
     */
    public SlartiInteger(final Long value) {
        super();
        this.value = Validate.notNull(value, "value");
    }

    @Override
    public SlartiNode eval(final Environment env) {
        return this;
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof SlartiInteger)) {
            return false;
        }

        final SlartiInteger that = (SlartiInteger) o;
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
    public Long value() {
        return value;
    }

    @Override
    public SlartiBoolean castToBoolean() {
        return ZERO_VALUE.equals(value) ? SlartiBoolean.FALSE : SlartiBoolean.TRUE;
    }

    @Override
    public SlartiInteger castToInteger() {
        return this;
    }

    @Override
    public SlartiReal castToReal() {
        return new SlartiReal(value.doubleValue());
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
