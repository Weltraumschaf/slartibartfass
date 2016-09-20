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
public final class SlartiInteger implements SlartiNode, SlartiType<Long> {
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
    public Object eval(final Environment env) {
        return value;
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
}
