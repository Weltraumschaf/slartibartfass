package de.weltraumschaf.slartibartfass.backend;

import de.weltraumschaf.slartibartfass.node.SlartiNode;

import java.util.Objects;

/**
 * This type represents a box for an allocated memory in the memory.
 *
 * @author Sven Strittmatter
 */
public final class MemoryBox {
    /**
     * The allocated memory.
     * <p>
     * This must never be {@code null} because all allocated values must be initialized. The memory may be changed later,
     * but never to {@code null}.
     * </p>
     */
    private SlartiNode<?> memory;

    /**
     * Dedicated constructor.
     *
     * @param memory must not be {@code null}
     */
    public MemoryBox(final SlartiNode<?> memory) {
        super();
        this.memory = Objects.requireNonNull(memory, "Parameter 'memory' must not be null!");
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
        return Objects.equals(memory, that.memory);
    }

    @Override
    public int hashCode() {
        return Objects.hash(memory);
    }

    @Override
    public String toString() {
        return "MemoryBox{"
            + "memory=" + memory
            + '}';
    }
}
