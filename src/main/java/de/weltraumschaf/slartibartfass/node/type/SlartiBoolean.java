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
public final class SlartiBoolean implements SlartiNode, SlartiType<Boolean> {
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
    public Object eval(final Environment env) {
        return value;
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
        return value ? "#true" : "#false";
    }

    @Override
    public Boolean value() {
        return value;
    }
}
