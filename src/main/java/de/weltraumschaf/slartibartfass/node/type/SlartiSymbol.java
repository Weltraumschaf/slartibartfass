package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

import java.util.Objects;

public final class SlartiSymbol implements SlartiNode {
    private final String name;

    public SlartiSymbol(final String name) {
        super();
        this.name = name;
    }

    public String name() {
        return name;
    }

    @Override
    public Object eval(final Environment env) {
        return env.getValue(name);
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof SlartiSymbol)) {
            return false;
        }

        final SlartiSymbol that = (SlartiSymbol) o;
        return Objects.equals(name, that.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public String toString() {
        return name;
    }
}
