package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

import java.util.Objects;

/**
 * BAse of all types which represent a value.
 *
 * @param <T> type of value
 * @author Sven Strittmatter
 */
abstract class ValueBasedType<T> implements SlartiNode<T> {

    /**
     * The bare value.
     */
    private final T value;

    /**
     * Dedicated constructor.
     *
     * @param value must not be {@code null}
     */
    ValueBasedType(final T value) {
        super();
        this.value = Validate.notNull(value, "value");
    }

    @Override
    public final SlartiNode eval(final Environment env) {
        return this;
    }

    @Override
    public final boolean equals(final Object o) {
        if (!(o instanceof ValueBasedType)) {
            return false;
        }

        final ValueBasedType that = (ValueBasedType) o;
        return Objects.equals(value, that.value);
    }

    @Override
    public final int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public String toString() {
        return "" + value;
    }

    @Override
    public final T value() {
        return value;
    }
}
