package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

import java.util.List;
import java.util.Objects;

/**
 * This is the base type for function invocations.
 * <p>
 *     {@link #eval(Environment) Evaluating} this node will return the node itself.
 * </p>
 */
public abstract class SlartiFunction implements SlartiNode {
    private final String name;

    /**
     * Dedicated constructor.
     *
     * @param name must not be {@code null} or empty
     */
    protected SlartiFunction(final String name) {
        super();
        this.name = Validate.notEmpty(name, "name");
    }

    @Override
    public final Object eval(final Environment env) {
        return this;
    }

    /**
     * Applies the function for the given arguments.
     *
     * @param args must not be {@code null}
     * @return never {@code null}
     */
    public abstract Object apply(final List<Object> args);

    /**
     * Whether the function is built in or user defined.
     *
     * @return {@code true} if built in, {@code false} for user defined
     */
    public abstract boolean isBuiltIn();

    /**
     * Get the literal name of the function.
     *
     * @return never {@code null} or empty
     */
    public final String name() {
        return name;
    }

    @Override
    public final boolean equals(final Object o) {
        if (!(o instanceof SlartiFunction)) {
            return false;
        }

        final SlartiFunction that = (SlartiFunction) o;
        return Objects.equals(name, that.name);
    }

    @Override
    public final int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public String toString() {
        return name;
    }
}
