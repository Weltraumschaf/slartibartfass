package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

import java.util.Objects;

/**
 * This type represents a box for an allocated value in the memory.
 *
 * @author Sven Strittmatter
 */
public final class MemoryBox {
    /**
     * Gives the slot a unique (in its environment) name to identify the allocated memory.
     * <p>
     * The symbol of a box must not be changed after allocated.
     * </p>
     */
    private final SlartiSymbol symbol;
    /**
     * The allocated value.
     * <p>
     * This must never be {@code null} because all allocated values must be initialized. The value may be changed later,
     * but never to {@code null}. The value is not considered in {@link #equals(Object)} and {@link #hashCode()} because
     * the value may change, but the allocated box is the same.
     * </p>
     */
    private transient SlartiNode<?> value;

    /**
     * Dedicated constructor.
     *
     * @param symbol must not be {@code null}
     * @param value  must not be {@code null}
     */
    public MemoryBox(final SlartiSymbol symbol, final SlartiNode<?> value) {
        super();
        this.symbol = Objects.requireNonNull(symbol, "Parameter 'symbol' must not be null!");
        this.value = Objects.requireNonNull(value, "Parameter 'value' must not be null!");
    }

    /**
     * Get the symbol of the box.
     * <p>
     * This value never change after object creation during its life time.
     * </p>
     *
     * @return never {@code null}
     */
    public SlartiSymbol getSymbol() {
        return symbol;
    }

    /**
     * Get the allocated value.
     * <p>
     * This value may change during object life  time.
     * </p>
     *
     * @return never {@code null}
     */
    public SlartiNode<?> getValue() {
        return value;
    }

    /**
     * Changes a allocated value.
     *
     * @param value must not be {@code null}
     */
    public void setValue(final SlartiNode<?> value) {
        this.value = Objects.requireNonNull(value, "Parameter 'value' must not be null!");
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof MemoryBox)) {
            return false;
        }

        final MemoryBox that = (MemoryBox) o;
        return Objects.equals(symbol, that.symbol);
    }

    @Override
    public int hashCode() {
        return Objects.hash(symbol);
    }

    @Override
    public String toString() {
        return "MemoryBox{" +
            "symbol=" + symbol +
            ", value=" + value +
            '}';
    }
}
