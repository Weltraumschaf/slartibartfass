package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.backend.Pair;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Objects;

/**
 * This is a simple list implementation which provides an interface to do Scheme like operations.
 * <p>
 * This list is a simply lonked list which provides {@link #head()} and {@link #tail()} operations.
 * </p>
 *
 * @author Sven Strittmatter
 */
public class InternalList implements Iterable<SlartiNode> {

    /**
     * Reusable empty list.
     */
    public static final InternalList EMPTY = new InternalList() {
        @Override
        public void add(final SlartiNode element) {
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
    private Pair head;
    /**
     * The last element.
     */
    private Pair last;

    /**
     * Dedicated constructor.
     *
     * @param elements must not be {@code null}
     */
    public InternalList(final Collection<SlartiNode> elements) {
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
    public void add(final SlartiNode element) {
        Validate.notNull(element, "element");
        final Pair newPair = Pair.cons(element);

        if (null == head) {
            head = newPair;
            last = head;
        } else {
            last.cdr(newPair);
            last = newPair;
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
    public final SlartiNode head() {
        if (null == head) {
            return null;
        }

        return head.car();
    }

    /**
     * Return the list except the {@link #head() first element}.
     *
     * @return never {@code null}, maybe empty if there are no more elements than the {@link #head()}
     */
    public final InternalList tail() {
        final InternalList tail = new InternalList();

        if (size > 1) {
            tail.size = size - 1;
            tail.head = head.cdr();
            tail.last = last;
        }

        return tail;
    }

    @Override
    public final Iterator<SlartiNode> iterator() {
        return new InternalIterator(head);
    }

    @Override
    public final boolean equals(final Object o) {
        if (!(o instanceof InternalList)) {
            return false;
        }

        final InternalList that = (InternalList) o;
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

        for (final SlartiNode item : this) {
            buffer.append(sep).append(item);
            sep = ", ";
        }

        return buffer.toString();
    }

    /**
     * Implements iterator for {@link InternalList}.
     */
    private static final class InternalIterator implements Iterator<SlartiNode> {

        /**
         * The current iterated entry.
         */
        private Pair current;

        /**
         * Dedicated constructor.
         *
         * @param head may be {@code null}
         */
        private InternalIterator(final Pair head) {
            super();
            this.current = head;
        }

        @Override
        public boolean hasNext() {
            return current != null && !current.isEmpty();
        }

        @Override
        public SlartiNode next() {
            final SlartiNode value = current.car();
            current = current.cdr();
            return value;
        }
    }
}
