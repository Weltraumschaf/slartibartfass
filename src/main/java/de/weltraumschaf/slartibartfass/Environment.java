package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.function.SlartiFunction;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

import java.io.PrintStream;
import java.util.*;

import static de.weltraumschaf.slartibartfass.node.Slarti.sym;

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
    private final Map<String, MemoryBox> store = new HashMap<>();
    /**
     * Null if the environment does not gave a parent.
     */
    private final Environment parent;

    /**
     * Convenience constuctor which creates one w/o a parent.
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

    public SlartiNode getValue(final String name) {
        if (store.containsKey(name)) {
            return this.store.get(name).getValue();
        } else if (hasParent()) {
            return parent.getValue(name);
        } else {
            throw new SlartiError(String.format("No symbol '%s' found!", name));
        }
    }

    public void putValue(final SlartiSymbol name, final SlartiNode value) {
        store.put(name.name(), new MemoryBox(name, value));
    }

    @Deprecated
    public void putValue(final String name, final SlartiNode value) {
        putValue(sym(name), value);
    }

    public boolean hasParent() {
        return null != parent;
    }

    public int size() {
        return store.size();
    }

    public void print(final PrintStream out) {
        if (parent != null) {
            parent.print(out);
        }

        final List<String> symbols = new ArrayList<>(store.keySet());
        Collections.sort(symbols);
        symbols.forEach(symbol -> out.println(String.format("  %1$-8s", symbol) + " -> " + format(store.get(symbol))));
    }

    private String format(final Object o) {
        if (o instanceof SlartiFunction) {
            final SlartiFunction fn = (SlartiFunction) o;
            return fn.isBuiltIn() ? "builtin" : "defined";
        }

        return o.toString();
    }

    @Override
    public String toString() {
        return "Environment{" +
            "store=" + store +
            ", parent=" + parent +
            '}';
    }
}
