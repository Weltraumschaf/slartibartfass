package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.slartibartfass.node.function.SlartiFunction;

import java.io.PrintStream;
import java.util.*;

public final class Environment {
    private final Map<String, Object> store = new HashMap<>();
    private final Environment parent;

    public Environment() {
        this(null);
    }

    public Environment(final Environment parent) {
        super();
        this.parent = parent;
    }

    public Object getValue(final String name) {
        if (store.containsKey(name)) {
            return this.store.get(name);
        } else if (hasParent()) {
            return parent.getValue(name);
        } else {
            throw new SlartiError(String.format("No symbol '%s' found!", name));
        }
    }

    public void putValue(final String name, final Object value) {
        store.put(name, value);
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
        symbols.forEach(symbol -> out.println(symbol + " -> " + format(store.get(symbol))));
    }

    private String format(final Object o) {
        if (o instanceof SlartiFunction) {
            final SlartiFunction fn = (SlartiFunction) o;
            return fn.isBuiltIn() ? "builtin fn" : "defined fn";
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
