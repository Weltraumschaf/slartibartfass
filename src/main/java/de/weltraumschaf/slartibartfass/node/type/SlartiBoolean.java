package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.SlartiType;

import java.util.Objects;

/**
 * Boolean type of the language.
 * <p>
 *     {@link #eval(Environment) Evaluating} this node will return its bare boolean representation.
 * </p>
 */
public final class SlartiBoolean implements SlartiNode<Boolean> {
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
    static final String TRUE_TOKEN = "#true";
    static final String FALSE_TOKEN = "#false";

    private final Boolean value;

    /**
     * Use either {@link #TRUE} or {@link #FALSE}.
     *
     * @param value must not be {@code null}
     */
    private SlartiBoolean(final Boolean value) {
        super();
        this.value = Validate.notNull(value, "value");
    }

    @Override
    public SlartiNode eval(final Environment env) {
        return this;
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof SlartiBoolean)) {
            return false;
        }

        final SlartiBoolean that = (SlartiBoolean) o;
        return Objects.equals(value, that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public String toString() {
        return Boolean.TRUE.equals(value) ? TRUE_TOKEN : FALSE_TOKEN;
    }

    @Override
    public Boolean value() {
        return value;
    }

    @Override
    public SlartiBoolean castToBoolean() {
        return this;
    }

    @Override
    public SlartiInteger castToInteger() {
        return Boolean.TRUE.equals(value) ? new SlartiInteger(1L) : new SlartiInteger(0L);
    }

    @Override
    public SlartiReal castToReal() {
        return Boolean.TRUE.equals(value) ? new SlartiReal(1d) : new SlartiReal(0d);
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
