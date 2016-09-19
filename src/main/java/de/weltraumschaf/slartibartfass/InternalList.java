package de.weltraumschaf.slartibartfass;

import java.util.Collection;
import java.util.Iterator;
import java.util.Objects;

public final class InternalList<T> implements Iterable<T> {

    public static final InternalList<?> EMPTY = new InternalList<>();

    private int size;
    private Pair<T> head;
    private Pair<T> last;

    public InternalList(final Collection<T> elements) {
        this();
        elements.forEach(this::add);
    }

    InternalList() {
        super();
    }

    void add(T element) {
        Objects.requireNonNull(element, "Parameter 'element' must not be null!");

        if (null == head) {
            head = new Pair<>(element);
            last = head;
        } else {
            last.next = new Pair<>(element);
            last = last.next;
        }

        ++size;
    }

    public int size() {
        return size;
    }

    public T head() {
        if (null == head) {
            return null;
        }

        return head.value;
    }

    public InternalList<T> tail() {
        final InternalList<T> tail = new InternalList<>();

        if (size > 1) {
            tail.size = size - 1;
            tail.head = head.next;
            tail.last = last;
        }

        return tail;
    }

    @Override
    public Iterator<T> iterator() {
        return new InternalIterato<>(head);
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof InternalList)) {
            return false;
        }

        final InternalList<?> that = (InternalList<?>) o;
        return size == that.size &&
            Objects.equals(head, that.head) &&
            Objects.equals(last, that.last);
    }

    @Override
    public int hashCode() {
        return Objects.hash(size, head, last);
    }

    @Override
    public String toString() {
        return new StringBuilder("InternalList{size=")
            .append(size).append(", items=[")
            .append(itemsAsString())
            .append("]}")
            .toString();
    }

    public String itemsAsString() {
        final StringBuilder buffer = new StringBuilder();
        String sep = "";

        for (final T item : this) {
            buffer.append(sep).append(item);
            sep = ", ";
        }

        return buffer.toString();
    }

    static final class Pair<V> {
        private final V value;
        private Pair<V> next;

        Pair(final V value) {
            super();
            this.value = value;
        }

        @Override
        public boolean equals(final Object o) {
            if (!(o instanceof Pair)) {
                return false;
            }

            final Pair<?> pair = (Pair<?>) o;
            return Objects.equals(value, pair.value) &&
                Objects.equals(next, pair.next);
        }

        @Override
        public int hashCode() {
            return Objects.hash(value, next);
        }
    }

    private static final class InternalIterato<E> implements Iterator<E> {

        private Pair<E> current;

        public InternalIterato(final Pair<E> head) {
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
