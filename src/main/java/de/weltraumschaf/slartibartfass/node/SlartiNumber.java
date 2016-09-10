package de.weltraumschaf.slartibartfass.node;

import de.weltraumschaf.slartibartfass.Environment;

import java.util.Objects;

public final class SlartiNumber implements SlartiNode {
    private final Long value;

    public SlartiNumber(Long value) {
        super();
        this.value = value;
    }

    @Override
    public Object eval(final Environment env) {
        return this.value;
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof SlartiNumber)) {
            return false;
        }

        final SlartiNumber that = (SlartiNumber) o;
        return Objects.equals(value, that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public String toString() {
        return "" + value;
    }
}
