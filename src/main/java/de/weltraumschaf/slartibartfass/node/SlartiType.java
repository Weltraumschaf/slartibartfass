package de.weltraumschaf.slartibartfass.node;

import de.weltraumschaf.commons.validate.Validate;
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
     * @return {@code true} if it is of the given type, else {@code false}
     */
    default boolean isOf(final Class<?> other) {
        return this.getClass().isAssignableFrom(other);
    }

    /**
     * Whether the implementation is of type boolean.
     * <p>
     * This is a convenience method for {@link #isOf(Class)} for type {@link SlartiBoolean}.
     * </p>
     *
     * @return {@code true} if it is a {@link SlartiBoolean}, else {@code false}
     */
    default boolean isBoolean() {
        return isOf(SlartiBoolean.class);
    }

    /**
     * Whether the implementation is of type integer.
     * <p>
     * This is a convenience method for {@link #isOf(Class)} for type {@link SlartiInteger}.
     * </p>
     *
     * @return {@code true} if it is a {@link SlartiInteger}, else {@code false}
     */
    default boolean isInteger() {
        return isOf(SlartiInteger.class);
    }

    /**
     * Whether the implementation is of type list.
     * <p>
     * This is a convenience method for {@link #isOf(Class)} for type {@link SlartiList}.
     * </p>
     *
     * @return {@code true} if it is a {@link SlartiList}, else {@code false}
     */
    default boolean isList() {
        return isOf(SlartiList.class);
    }

    /**
     * Whether the implementation is of type real.
     * <p>
     * This is a convenience method for {@link #isOf(Class)} for type {@link SlartiReal}.
     * </p>
     *
     * @return {@code true} if it is a {@link SlartiReal}, else {@code false}
     */
    default boolean isReal() {
        return isOf(SlartiReal.class);
    }

    /**
     * Whether the implementation is of type string.
     * <p>
     * This is a convenience method for {@link #isOf(Class)} for type {@link SlartiString}.
     * </p>
     *
     * @return {@code true} if it is a {@link SlartiString}, else {@code false}
     */
    default boolean isString() {
        return isOf(SlartiString.class);
    }

    /**
     * Whether the implementation is of type symbol.
     * <p>
     * This is a convenience method for {@link #isOf(Class)} for type {@link SlartiSymbol}.
     * </p>
     *
     * @return {@code true} if it is a {@link SlartiSymbol}, else {@code false}
     */
    default boolean isSymbol() {
        return isOf(SlartiSymbol.class);
    }

    /**
     * Get the native value of the type.
     * <p>
     * Returns {@code null} by default.
     * </p>
     *
     * @return maybe {@code null}
     */
    default T value() {
        return null;
    }

    /**
     * Implementors can implement this method to cast it self to a boolean.
     * <p>
     * Throws a {@link SlartiError} by default.
     * </p>
     *
     * @return never {@code null}
     */
    default SlartiBoolean castToBoolean() {
        throw unsupportedCastError(SlartiBoolean.class);
    }

    /**
     * Implementors can implement this method to cast it self to a integer.
     * <p>
     * Throws a {@link SlartiError} by default.
     * </p>
     *
     * @return never {@code null}
     */
    default SlartiInteger castToInteger() {
        throw unsupportedCastError(SlartiInteger.class);
    }

    /**
     * Implementors can implement this method to cast it self to a real.
     * <p>
     * Throws a {@link SlartiError} by default.
     * </p>
     *
     * @return never {@code null}
     */
    default SlartiReal castToReal() {
        throw unsupportedCastError(SlartiReal.class);
    }

    /**
     * Implementors can implement this method to cast it self to a string.
     * <p>
     * Throws a {@link SlartiError} by default.
     * </p>
     *
     * @return never {@code null}
     */
    default SlartiString castToString() {
        throw unsupportedCastError(SlartiString.class);
    }

    /**
     * Implementors can implement this method to cast it self to a list.
     * <p>
     * Throws a {@link SlartiError} by default.
     * </p>
     *
     * @return never {@code null}
     */
    default SlartiList castToList() {
        throw unsupportedCastError(SlartiList.class);
    }

    /**
     * Tries to cast to the given objects class.
     * <p>
     * May throw an {@link SlartiError} if it can't cast.
     * </p>
     *
     * @param wanted must not be {@code null}
     * @return never {@code null}
     */
    default SlartiNode castTo(final SlartiType wanted) {
        Validate.notNull(wanted, "wanted");

        if (wanted.isBoolean()) {
            return castToBoolean();
        } else if (wanted.isInteger()) {
            return castToInteger();
        } else if (wanted.isReal()) {
            return castToReal();
        } else if (wanted.isString()) {
            return castToString();
        } else if (wanted.isList()) {
            return castToList();
        } else {
            throw unsupportedCastError(wanted.getClass());
        }
    }

    /**
     * Convenience method do create an error fo cast to unsupported type.
     *
     * @param wanted must not be {@code null}
     * @return never {@code null}
     */
    default SlartiError unsupportedCastError(final Class<?> wanted) {
        Validate.notNull(wanted, "wanted");
        return new SlartiError(
            String.format("%s does not support cast to %s!",
                getClass().getSimpleName(), wanted.getSimpleName()));
    }

}
