package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

import java.util.Objects;

/**
 * String type of the language.
 * <p>
 *     {@link #eval(Environment) Evaluating} this node will return its bare string representation.
 * </p>
 */
public final class SlartiString implements SlartiNode {
    private final String value;

    /**
     * Dedicated constructor.
     *
     * @param value must not be {@code null}
     */
    public SlartiString(final String value) {
        super();
        this.value = Validate.notNull(value, "value");
    }

    @Override
    public Object eval(final Environment env) {
        return value;
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof SlartiString)) {
            return false;
        }

        final SlartiString that = (SlartiString) o;
        return Objects.equals(value, that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public String toString() {
        return '"' + value + '"';
    }
}
