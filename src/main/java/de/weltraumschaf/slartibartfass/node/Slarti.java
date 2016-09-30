package de.weltraumschaf.slartibartfass.node;

import de.weltraumschaf.slartibartfass.node.type.*;

import java.util.Collection;

/**
 * Factory to create nodes.
 *
 * @author Sven Strittmatter
 */
public final class Slarti {

    /**
     * Hidden for pure static factory class.
     */
    private Slarti() {
        super();
    }

    /**
     * Convenience method to create booleans.
     *
     * @param value must not be {@code null}
     * @return never {@code null}
     */
    public static SlartiBoolean of(final Boolean value) {
        return value ? SlartiBoolean.TRUE : SlartiBoolean.FALSE;
    }

    /**
     * Convenience method to create integers.
     *
     * @param value must not be {@code null}
     * @return never {@code null}
     */
    public static SlartiInteger of(final Long value) {
        return new SlartiInteger(value);
    }

    /**
     * Convenience method to create reals.
     *
     * @param value must not be {@code null}
     * @return never {@code null}
     */
    public static SlartiReal of(final Double value) {
        return new SlartiReal(value);
    }

    /**
     * Convenience method to create strings.
     *
     * @param value must not be {@code null}
     * @return never {@code null}
     */
    public static SlartiString of(final String value) {
        return new SlartiString(value);
    }

    /**
     * Convenience method to create a list from nodes.
     *
     * @param nodes must not be {@code null}
     * @return never {@code null}
     */
    public static SlartiList list(final SlartiNode ... nodes) {
        return new SlartiList(nodes);
    }

    public static SlartiList list(final Collection<SlartiNode> nodes) {
        return new SlartiList(nodes);
    }

    /**
     * Convenience method to create a symbol.
     *
     * @param name must not be {@code null} or empty
     * @return never {@code null}
     */
    public static SlartiSymbol sym(final String name) {
        return new SlartiSymbol(name);
    }
}
