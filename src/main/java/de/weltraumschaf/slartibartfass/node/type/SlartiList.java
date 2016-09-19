package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.InternalList;
import de.weltraumschaf.slartibartfass.node.function.SlartiFunction;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

import java.util.*;

public class SlartiList implements SlartiNode, Iterable<SlartiNode> {
    public static final SlartiList EMPTY = new SlartiList(Collections.emptyList());
    protected final InternalList<SlartiNode> data;

    public SlartiList(final SlartiNode ... data) {
        this(Arrays.asList(data));
    }

    public SlartiList(Collection<SlartiNode> data) {
        this(new InternalList<>(data));
    }

    public SlartiList(InternalList<SlartiNode> data) {
        super();
        this.data = data;
    }

    @Override
    public Object eval(Environment env) {
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

    public InternalList<SlartiNode> data() {
        return data;
    }

    public SlartiNode head() {
        return data.head();
    }

    public SlartiList tail() {
        return new SlartiList(data.tail());
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof SlartiList)) {
            return false;
        }

        final SlartiList list = (SlartiList) o;
        return Objects.equals(data, list.data);
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
    public Iterator<SlartiNode> iterator() {
        return data.iterator();
    }

    public int size() {
        return data.size();
    }
}
