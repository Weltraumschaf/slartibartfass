package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

import java.util.Objects;

/**
 * String type of the language.
 * <p>
 * {@link #eval(Environment) Evaluating} this node will return its bare string representation.
 * </p>
 *
 * @author Sven Strittmatter
 */
public final class SlartiString extends ValueBasedType<String> {

    /**
     * Dedicated constructor.
     *
     * @param value must not be {@code null}
     */
    public SlartiString(final String value) {
        super(value);
    }

    @Override
    public SlartiBoolean castToBoolean() {
        return SlartiBoolean.TRUE_TOKEN.equals(value()) ? SlartiBoolean.TRUE : SlartiBoolean.FALSE;
    }

    @Override
    public SlartiInteger castToInteger() {
        try {
            return new SlartiInteger(Double.valueOf(value()).longValue());
        } catch (final NumberFormatException e) {
            return SlartiInteger.ZERO;
        }
    }

    @Override
    public SlartiReal castToReal() {
        try {
            return new SlartiReal(Double.valueOf(value()));
        } catch (final NumberFormatException e) {
            return SlartiReal.ZERO;
        }
    }

    @Override
    public SlartiString castToString() {
        return this;
    }

    @Override
    public SlartiList castToList() {
        return new SlartiList(this);
    }

    @Override
    public String toString() {
        return '"' + value() + '"';
    }
}
