package de.weltraumschaf.slartibartfass.backend;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

import java.util.Objects;

/**
 * This represents a classic LISP style pair.
 * <p>
 * A pair os a box which stores a value and a pointer to a potential next value.
 * </p>
 *
 * @author Sven Strittmatter
 */
public final class Pair {

    /**
     * Used to signal that there is no nex value.
     */
    private static final Pair EMPTY = new Pair();
    /**
     * The stored element.
     */
    private final SlartiNode car;
    /**
     * The pointer to the next pair.
     */
    private Pair cdr;

    /**
     * Convenience constructor for empty pair.
     */
    private Pair() {
        this(null, null);
    }

    /**
     * Dedicated constructor.
     *
     * @param car may be {@code null}
     * @param cdr may be {@code null}
     */
    private Pair(final SlartiNode car, final Pair cdr) {
        super();
        this.car = car;
        this.cdr = cdr;
    }

    /**
     * This method gives the stored content of the pair.
     * <p>
     * Throws {@link IllegalStateException} if this pair is the special empty pair.
     * </p>
     * <p>
     * The name <em>car</em> derive from the original implementation of Lisp on the <em>IBM 704</em>. That machine had
     * an addressing scheme that allowed one to reference the “address” and “decrement” parts of a memory location.
     * <em>car</em> stands for “Contents of Address part of Register”.
     * </p>
     *
     * @return never {@code null}
     */
    public SlartiNode car() {
        if (isEmpty()) {
            throw new IllegalStateException("You must not call cdr() on the empty pair!");
        }

        return car;
    }

    /**
     * This method gives the next linked pair.
     * <p>
     * If there is {@link #hasCdr() no cdr} then this method returns a special empty object for which {@link #isEmpty()}
     * is always {@code true}. Throws {@link IllegalStateException} if this pair is the special empty pair.
     * </p>
     * <p>
     * The names <em>cdr</em>derive from the original implementation of Lisp on the <em>IBM 704</em>. That machine had
     * an addressing scheme that allowed one to reference the “address” and “decrement” parts of a memory location.
     * <em>cdr</em> stands for “Contents of Decrement part of Register”.
     * </p>
     *
     * @return never {@code null}
     */
    public Pair cdr() {
        if (isEmpty()) {
            throw new IllegalStateException("You must not call cdr() on the empty pair!");
        }

        return cdr;
    }

    public void cdr(final Pair cdr) {
        this.cdr = Validate.notNull(cdr, "cdr");
    }

    /**
     * Returns if this pair has a <em>cdr</em>.
     * <p>
     * If this method returns {@code false} then {@link #cdr()} returns either an special object or on that special
     * empty object {@code null}.
     * </p>
     *
     * @return {@code true} if there is a pair as cdr with a value, else {@code false}
     */
    public boolean hasCdr() {
        return cdr != EMPTY && cdr != null;
    }

    /**
     * Returns if this is the special empty pair.
     * <p>
     * This empty pair signals that there is no car/cdr anymore. Calling {@link #car()} or {@link #cdr()} will throw
     * {@link IllegalStateException} if called on the empty pair.
     * </p>
     *
     * @return
     */
    public boolean isEmpty() {
        return this == EMPTY;
    }

    @Override
    public boolean equals(final Object other) {
        if (!(other instanceof Pair)) {
            return false;
        }

        final Pair that = (Pair) other;
        return Objects.equals(car, that.car) &&
            Objects.equals(cdr, that.cdr);
    }

    @Override
    public int hashCode() {
        return Objects.hash(car, cdr);
    }

    @Override
    public String toString() {
        return "Pair{" +
            "car=" + car +
            ", cdr=" + cdr +
            '}';
    }

    /**
     * Construct a pair from a single node.
     *
     * @param car must not be {@code null}
     * @return never {@code null}
     */
    public static Pair cons(final SlartiNode car) {
        return cons(car, EMPTY);
    }

    /**
     * Constructs a pair from the gicen node with the given pair as the next pair.
     *
     * @param car must not be {@code null}
     * @param cdr must not be {@code null}
     * @return never {@code null}
     */
    public static Pair cons(final SlartiNode car, final Pair cdr) {
        return new Pair(Validate.notNull(car, "car"), Validate.notNull(cdr, "cdr"));
    }

    /**
     * Creates a list by recursive cons'ing up the nodes.
     * <p>
     * Given nodes {@code Pair.list(of("foo"), of("bar"), of("baz"))} become a structure like:
     * </p>
     * <pre>
     * +----------+    +----------+    +----------+
     * | "foo"  *-+--->| "bar"  *-+--->| "baz"  *-+--->[EMPTY]
     * +----------+    +----------+    +----------+
     * </pre>
     *
     * @param nodes must not be {@code null} or empty
     * @return never {@code null}
     */
    public static Pair list(final SlartiNode... nodes) {
        Validate.notNull(nodes, "nodes");

        if (nodes.length == 0) {
            throw new IllegalArgumentException("No nodes given!");
        }

        Pair tmp = EMPTY;

        for (int i = nodes.length - 1; i >= 0; --i) {
            tmp = cons(nodes[i], tmp);
        }

        return tmp;
    }
}
