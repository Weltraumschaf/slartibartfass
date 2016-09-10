package de.weltraumschaf.slartibartfass.node;

import de.weltraumschaf.slartibartfass.Environment;

import java.util.List;
import java.util.Objects;

public abstract class SlartiFunction implements SlartiNode {
    private final String name;

    protected SlartiFunction(String name) {
        super();
        this.name = name;
    }

    @Override
    public final Object eval(Environment env) {
        return this;
    }

    public abstract Object apply(final List<Object> args);

    public final String name() {
        return name;
    }

    @Override
    public final boolean equals(final Object o) {
        if (!(o instanceof SlartiFunction)) {
            return false;
        }

        final SlartiFunction that = (SlartiFunction) o;
        return Objects.equals(name, that.name);
    }

    @Override
    public final int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public String toString() {
        return name;
    }
}
