package de.weltraumschaf.slartibartfass.node;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.InternalList;

import java.util.*;
import java.util.stream.Collectors;

public class SlartiList implements SlartiNode, Iterable<SlartiNode> {
    public static final SlartiList EMPTY = new SlartiList(Collections.emptyList());
    protected final InternalList<SlartiNode> data;

    public SlartiList(final SlartiNode ... data) {
        this(Arrays.asList(data));
    }

    public SlartiList(List<SlartiNode> data) {
        this(new InternalList<>(data));
    }

    public SlartiList(InternalList<SlartiNode> data) {
        super();
        this.data = data;
    }

    @Override
    public Object eval(Environment env) {
        final SlartiFunction function = (SlartiFunction) head().eval(env);
        final List<Object> args = new ArrayList<>();

        for (final SlartiNode node : this.tail()) {
            args.add(node.eval(env));
        }

        return function.apply(args);
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
