package de.weltraumschaf.slartibartfass.node;

/**
 *
 * @param <T> underlying raw Java type
 */
public interface SlartiType<T> {

    default boolean isOf(final Class<?> other) {
        return this.getClass().isAssignableFrom(other);
    }

    T value();
}
