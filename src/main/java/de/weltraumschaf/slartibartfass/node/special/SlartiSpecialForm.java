package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.node.SlartiList;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.SlartiSymbol;

import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The special forms contains a list of expressions (the name is not part of the list).
 */
public abstract class SlartiSpecialForm extends SlartiList {
    static final SlartiSymbol DEFINE = new SlartiSymbol("define");
    static final SlartiSymbol LAMBDA = new SlartiSymbol("lambda");
    static final SlartiSymbol IF = new SlartiSymbol("if");
    static final SlartiSymbol QUOTE = new SlartiSymbol("quote");
    private final SlartiSymbol name;

    public SlartiSpecialForm(final SlartiSymbol name, final SlartiList list) {
        super(list.data());
        this.name = name;
    }

    public static SlartiNode check(final SlartiList list) {
        if (list == SlartiList.EMPTY) {
            return list;
        } else {
            final SlartiNode head = list.head();
            final SlartiList tail = list.tail();

            if (DEFINE.equals(head)) {
                return new DefineSpecialForm(tail);
            } else if (LAMBDA.equals(head)) {
                return new LambdaSpecialForm(tail);
            } else if (IF.equals(head)) {
                return new IfSpecialForm(tail);
            } else if (QUOTE.equals(head)) {
                return new QuoteSpecialForm(tail);
            }
        }

        return list;
    }

    @Override
    public final boolean equals(final Object o) {
        if (!(o instanceof SlartiSpecialForm)) {
            return false;
        }

        final SlartiSpecialForm that = (SlartiSpecialForm) o;
        return Objects.equals(name, that.name) &&
            Objects.equals(data, that.data);
    }

    @Override
    public final int hashCode() {
        return Objects.hash(name, data);
    }

    @Override
    public final String toString() {
        return '(' + name.toString() + ' ' + String.join(", ", data.stream().map(SlartiNode::toString).collect(Collectors.toList())) + ')';
    }
}
