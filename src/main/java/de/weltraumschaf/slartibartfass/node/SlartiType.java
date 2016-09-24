package de.weltraumschaf.slartibartfass.node;

import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.node.type.*;

/**
 * Capabilities of the languages native types.
 *
 * @param <T> underlying raw Java type
 * @author Sven Strittmatter
 */
public interface SlartiType<T> {

    /**
     * Method to determine if the type is of a certain class.
     *
     * @param other may be {@code null}
     * @return {@code true} if it is of the given type, else {@code false'}
     */
    default boolean isOf(final Class<?> other) {
        return this.getClass().isAssignableFrom(other);
    }

    default boolean isBoolean() {
        return isOf(SlartiBoolean.class);
    }

    default boolean isInteger() {
        return isOf(SlartiInteger.class);
    }

    default boolean isList() {
        return isOf(SlartiList.class);
    }

    default boolean isReal() {
        return isOf(SlartiReal.class);
    }

    default boolean isString() {
        return isOf(SlartiString.class);
    }

    default boolean isSymbol() {
        return isOf(SlartiSymbol.class);
    }

    default T value() { return null; }

    default SlartiBoolean castToBoolean() { throw unsupportedCastError(SlartiBoolean.class); }
    default SlartiInteger castToInteger() { throw unsupportedCastError(SlartiInteger.class); }
    default SlartiReal castToReal() { throw unsupportedCastError(SlartiReal.class); }
    default SlartiString castToString() { throw unsupportedCastError(SlartiString.class); }
    default SlartiList castToList() { throw unsupportedCastError(SlartiList.class); }

    default SlartiError unsupportedCastError(final Class<?> wanted) {
        return new SlartiError(
            String.format("%s does not support cast to %s!",
                getClass().getSimpleName(), wanted.getSimpleName()));
    }

}
