package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.validate.Validate;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Objects;

/**
 * This is a simple list implementation which provides an interface to do Scheme like operations.
 * <p>
 *     This list is a simply lonked list which provides {@link #head()} and {@link #tail()} operations.
 * </p>
 *
 * @param <T> type of the elements in the list
 * @author Sven Strittmatter
 */
public class InternalList<T> implements Iterable<T> {

    /**
     * Reusable empty list.
     */
    public static final InternalList<?> EMPTY = new InternalList<Object>() {
        @Override
        public void add(final Object element) {
            throw new UnsupportedOperationException("Adding elements not allowed!");
        }
    };

    /**
     * Number of elements in the list.
     */
    private int size;
    /**
     * The first element.
     */
    private LinkedEntry<T> head;
    /**
     * The last element.
     */
    private LinkedEntry<T> last;

    /**
     * Dedicated constructor.
     *
     * @param elements must not be {@code null}
     */
    public InternalList(final Collection<T> elements) {
        super();
        elements.forEach(this::add);
    }

    /**
     * Convenience constructor for empty list.
     */
    public InternalList() {
        this(Collections.emptyList());
    }

    /**
     * Add a new element to the end of the list.
     *
     * @param element must not be {@code null}
     */
    public void add(final T element) {
        Validate.notNull(element, "element");

        if (null == head) {
            head = new LinkedEntry<>(element);
            last = head;
        } else {
            last.next = new LinkedEntry<>(element);
            last = last.next;
        }

        ++size;
    }

    /**
     * Returns the number of elements in the list.
     *
     * @return never negative
     */
    public final int size() {
        return size;
    }

    /**
     * Returns the first element of the list.
     *
     * @return {@code null} if the list is empty
     */
    public final T head() {
        if (null == head) {
            return null;
        }

        return head.value;
    }

    /**
     * Return the list except the {@link #head() first element}.
     *
     * @return never {@code null}, maybe empty if there are no more elements than the {@link #head()}
     */
    public final InternalList<T> tail() {
        final InternalList<T> tail = new InternalList<>();

        if (size > 1) {
            tail.size = size - 1;
            tail.head = head.next;
            tail.last = last;
        }

        return tail;
    }

    @Override
    public final Iterator<T> iterator() {
        return new InternalIterator<>(head);
    }

    @Override
    public final boolean equals(final Object o) {
        if (!(o instanceof InternalList)) {
            return false;
        }

        final InternalList<?> that = (InternalList<?>) o;
        return size == that.size
            && Objects.equals(head, that.head)
            && Objects.equals(last, that.last);
    }

    @Override
    public final int hashCode() {
        return Objects.hash(size, head, last);
    }

    @Override
    public final String toString() {
        return new StringBuilder("InternalList{size=")
            .append(size).append(", items=[")
            .append(itemsAsString())
            .append("]}")
            .toString();
    }

    /**
     * Get the list itmes coma separated.
     *
     * @return never {@code null} maybe empty
     */
    public final String itemsAsString() {
        final StringBuilder buffer = new StringBuilder();
        String sep = "";

        for (final T item : this) {
            buffer.append(sep).append(item);
            sep = ", ";
        }

        return buffer.toString();
    }

    /**
     * Entry type to link the list values.
     *
     * @param <V> type of entry
     */
    static final class LinkedEntry<V> {
        /**
         * The value itself.
         * <p>
         *     This value is never {@code null}
         * </p>
         */
        private final V value;
        /**
         * The next linked value.
         * <p>
         *     This value may {@code null} for the last one in the list.
         * </p>
         */
        private LinkedEntry<V> next;

        /**
         * Dedicated constructor.
         *
         * @param value must not be {@code null}
         */
        LinkedEntry(final V value) {
            super();
            this.value = Validate.notNull(value, "value");
        }

        @Override
        public boolean equals(final Object o) {
            if (!(o instanceof InternalList.LinkedEntry)) {
                return false;
            }

            final LinkedEntry<?> linkedEntry = (LinkedEntry<?>) o;
            return Objects.equals(value, linkedEntry.value)
                && Objects.equals(next, linkedEntry.next);
        }

        @Override
        public int hashCode() {
            return Objects.hash(value, next);
        }
    }

    /**
     * Implememts iterator for {@link InternalList}.
     *
     * @param <E> type of iterated elements
     */
    private static final class InternalIterator<E> implements Iterator<E> {

        /**
         * The current iterated entry.
         */
        private LinkedEntry<E> current;

        /**
         * Dedicated constructor.
         *
         * @param head may be {@code null}
         */
        private InternalIterator(final LinkedEntry<E> head) {
            super();
            this.current = head;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public E next() {
            final E value = current.value;
            current = current.next;
            return value;
        }
    }
}
