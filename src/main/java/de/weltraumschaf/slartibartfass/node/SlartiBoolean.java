package de.weltraumschaf.slartibartfass.node;

import de.weltraumschaf.slartibartfass.Environment;

import java.util.Objects;

public final class SlartiBoolean implements SlartiNode {
    public static final SlartiBoolean TRUE = new SlartiBoolean(Boolean.TRUE);
    public static final SlartiBoolean FALSE = new SlartiBoolean(Boolean.FALSE);

    private final Boolean value;

    private SlartiBoolean(final Boolean value) {
        super();
        this.value = value;
    }

    @Override
    public Object eval(final Environment env) {
        return value;
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof SlartiBoolean)) {
            return false;
        }

        final SlartiBoolean that = (SlartiBoolean) o;
        return Objects.equals(value, that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    @Override
    public String toString() {
        if (value) {
            return "#true";
        }

        return "#false";
    }
}
