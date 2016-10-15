package de.weltraumschaf.slartibartfass.backend;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.Ansi;
import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.function.SlartiFunction;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

import java.io.PrintStream;
import java.util.*;

/**
 * The environment stores allocated memory.
 * <p>
 * Allocated memory means everything which is defined by the {@link de.weltraumschaf.slartibartfass.node.special.DefineSpecialForm}.
 * The environment may have parents to build scopes. Typical the {@link SlartiFunction} builds his own environment as scope.
 * </p>
 *
 * @author Sven Strittmatter
 */
public final class Environment {
    /**
     * Stores the allocated memory.
     */
    private final Map<SlartiSymbol, MemoryBox> store = new HashMap<>();
    /**
     * Null if the environment does not gave a parent.
     */
    private final Environment parent;

    /**
     * Convenience constructor which creates one w/o a parent.
     */
    public Environment() {
        this(null);
    }

    /**
     * Dedicated constructor.
     *
     * @param parent may be {@code null}
     */
    public Environment(final Environment parent) {
        super();
        this.parent = parent;
    }

    /**
     * Get a stored value.
     * <p>
     * This method looks also in the parent environment if it is not found local and there is a parent. If the value is
     * not found anywhere a {@link SlartiError} will be thrown.
     * </p>
     *
     * @param name must not be {@code null}
     * @return never {@code null}
     */
    public MemoryBox getValue(final SlartiSymbol name) {
        Validate.notNull(name, "name");

        if (store.containsKey(name)) {
            return this.store.get(name);
        } else if (hasParent()) {
            return parent.getValue(name);
        } else {
            throw new SlartiError(String.format("No symbol '%s' found!", name));
        }
    }

    /**
     * Store a value in the environment.
     *
     * @param name  must not be {@code null}
     * @param value must not be {@code null}
     */
    public void putValue(final SlartiSymbol name, final SlartiNode value) {
        Validate.notNull(name, "name");
        Validate.notNull(value, "value");
        store.put(name, new MemoryBox(value));
    }

    /**
     * Whether the environment has a paent or not.
     *
     * @return {@code true} if it has one, else {@code false}
     */
    public boolean hasParent() {
        return null != parent;
    }

    /**
     * The number of allocated names.
     * <p>
     * This counts only the local allocated names excluding the ones allocated  in parents.
     * </p>
     *
     * @return not negative
     */
    public int size() {
        return store.size();
    }

    /**
     * Print all allocated symbols and its memory to the given print stream.
     * <p>
     * The printed symbols are sorted lexicographic. Also the dta of parents are printed first.
     * </p>
     *
     * @param out must not be {@code null}
     */
    public void print(final PrintStream out) {
        Validate.notNull(out, "out");

        if (parent != null) {
            parent.print(out);
        }

        final List<SlartiSymbol> symbols = new ArrayList<>(store.keySet());
        Collections.sort(symbols, (o1, o2) -> o1.name().compareTo(o2.name()));
        symbols.stream()
            .map(symbol -> {
                return "  " + Ansi.fmt().bold().text(String.format("%1$-8s", symbol.name().replaceAll("%", "%%"))).reset().toString()
                    + " -> " + format(store.get(symbol).memory());
            })
            .forEach(out::println);
    }

    /**
     * Formats the given nodes.
     *
     * @param node must not be {@code null}
     * @return never {@code null} or empty
     */
    private String format(final SlartiNode node) {
        if (node instanceof SlartiFunction) {
            final SlartiFunction fn = (SlartiFunction) node;
            return fn.isBuiltIn()
                ? Ansi.fmt().fg(Ansi.Color.BLUE).text("builtin fn").reset().toString()
                : "defined fn";
        }

        return node.toString();
    }

    @Override
    public String toString() {
        return "Environment{" +
            "store=" + store +
            ", parent=" + parent +
            '}';
    }
}
