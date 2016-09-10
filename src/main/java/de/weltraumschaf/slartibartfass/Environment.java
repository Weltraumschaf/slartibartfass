package de.weltraumschaf.slartibartfass;

import java.util.HashMap;
import java.util.Map;

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
        } else if (parent != null) {
            return parent.getValue(name);
        } else {
            throw new RuntimeException(String.format("No symbol '%s' found!", name));
        }
    }

    public void putValue(final String name, final Object value) {
        store.put(name, value);
    }

    @Override
    public String toString() {
        return "Environment{" +
            "store=" + store +
            ", parent=" + parent +
            '}';
    }
}
