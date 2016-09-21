package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.InternalList;
import de.weltraumschaf.slartibartfass.node.function.SlartiFunction;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

import java.util.*;

/**
 * List type of the language.
 * <p>
 *     This is the most complex type which represents a list of nodes. {@link #eval(Environment) Evaluating} this node
 *     will first look if {@link #head() the head} of the list is a {@link SlartiSymbol symbol} and if it is one resolve
 *     it in the {@link Environment environment}. If it is present it is checked if it is a {@link SlartiFunction function}
 *     and if apply it to the {@link #tail() tail} of the list. If it is not a symbol the resolved value will be returned.
 *     If the {@link #head() head} is not a symbol then the whole list is evaluated node by node and the aggregated result
 *     will be returned as collection. If the result is a single value then the collection will be unwrapped.
 * </p>
 *
 * TODO Add type interface.
 */
public class SlartiList implements SlartiNode, Iterable<SlartiNode> {
    /**
     * Represents an empty list.
     */
    public static final SlartiList EMPTY = new SlartiList(Collections.emptyList());
    private final InternalList<SlartiNode> data;

    public SlartiList(final SlartiNode ... data) {
        this(Arrays.asList(data));
    }

    public SlartiList(Collection<SlartiNode> data) {
        this(new InternalList<>(data));
    }

    /**
     * Dedicated constructor.
     *
     * @param data must not be {@code null}
     */
    public SlartiList(final InternalList<SlartiNode> data) {
        super();
        this.data = Validate.notNull(data, "data");
    }

    @Override
    public Object eval(final Environment env) {
        if (isHeadSymbol()) {
            return evalHead(env);
        }

        return evalAll(env);
    }

    private boolean isHeadSymbol() {
        return head() instanceof SlartiSymbol;
    }

    private Object evalHead(final Environment env) {
        final Object headResult = head().eval(env);

        if (headResult instanceof SlartiFunction) {
            final SlartiFunction function = (SlartiFunction) headResult;
            final List<Object> args = new ArrayList<>();

            for (final SlartiNode node : this.tail()) {
                args.add(node.eval(env));
            }

            return function.apply(args);
        }

        return headResult;
    }

    private Object evalAll(final Environment env) {
        final Collection<Object> results = new ArrayList<>();

        for (final SlartiNode node : data()) {
            final Object result = node.eval(env);

            if (SlartiList.EMPTY.equals(result)) {
                continue;
            }

            results.add(result);
        }

        return unwrapResult(new InternalList<>(results));
    }

    private Object unwrapResult(final InternalList<Object> results) {
        if (results.size() == 1) {
            return results.head();
        }

        return results;
    }

    /**
     * Returns all elements of the list.
     *
     * @return never {@code null}, no defensive copy
     */
    public final InternalList<SlartiNode> data() {
        return data;
    }

    /**
     * Return the first element of the list.
     *
     * @return may be {@code null}
     */
    public final SlartiNode head() {
        return data.head();
    }

    /**
     * Returns the whole list except the head.
     *
     * @return never {@code null}
     */
    public final SlartiList tail() {
        return new SlartiList(data.tail());
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof SlartiList)) {
            return false;
        }

        final SlartiList other = (SlartiList) o;
        return other.canEqual(this)
            && Objects.equals(data, other.data);
    }

    public boolean canEqual(final Object other) {
        return other instanceof SlartiList;
    }

    @Override
    public int hashCode() {
        return Objects.hash(data);
    }

    @Override
    public String toString() {
        return '(' + data.itemsAsString() + ')';
    }

    @Override
    public final Iterator<SlartiNode> iterator() {
        return data.iterator();
    }

    public final int size() {
        return data.size();
    }
}
