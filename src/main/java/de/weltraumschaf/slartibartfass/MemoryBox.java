package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

import java.util.Objects;

/**
 * This type represents a box for an allocated memory in the memory.
 *
 * @author Sven Strittmatter
 */
public final class MemoryBox {
    /**
     * Gives the slot a unique (in its environment) symbol to identify the allocated memory.
     * <p>
     * The symbol of a box must not be changed after allocated.
     * </p>
     */
    private final SlartiSymbol symbol;
    /**
     * The allocated memory.
     * <p>
     * This must never be {@code null} because all allocated values must be initialized. The memory may be changed later,
     * but never to {@code null}. The memory is not considered in {@link #equals(Object)} and {@link #hashCode()} because
     * the memory may change, but the allocated box is the same.
     * </p>
     */
    private transient SlartiNode<?> memory;

    /**
     * Dedicated constructor.
     *
     * @param symbol must not be {@code null}
     * @param memory  must not be {@code null}
     */
    public MemoryBox(final SlartiSymbol symbol, final SlartiNode<?> memory) {
        super();
        this.symbol = Objects.requireNonNull(symbol, "Parameter 'symbol' must not be null!");
        this.memory = Objects.requireNonNull(memory, "Parameter 'memory' must not be null!");
    }

    /**
     * Get the symbol of the box.
     * <p>
     * This memory never change after object creation during its life time.
     * </p>
     *
     * @return never {@code null}
     */
    public SlartiSymbol symbol() {
        return symbol;
    }

    /**
     * Get the allocated memory.
     * <p>
     * This memory may change during object life  time.
     * </p>
     *
     * @return never {@code null}
     */
    public SlartiNode<?> memory() {
        return memory;
    }

    /**
     * Changes a allocated memory.
     *
     * @param memory must not be {@code null}
     */
    public void memory(final SlartiNode<?> memory) {
        this.memory = Objects.requireNonNull(memory, "Parameter 'memory' must not be null!");
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
            ", memory=" + memory +
            '}';
    }
}
