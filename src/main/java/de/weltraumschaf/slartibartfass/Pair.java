package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

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
    private final Pair cdr;

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

    public SlartiNode car() {
        return car;
    }

    public Pair cdr() {
        return cdr;
    }

    public boolean hasCdr() {
        return cdr != EMPTY && cdr != null;
    }

    public boolean isEmpty() {
        return this == EMPTY;
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
