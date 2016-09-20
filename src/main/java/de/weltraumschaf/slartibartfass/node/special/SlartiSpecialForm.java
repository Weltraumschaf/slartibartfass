package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

import java.util.Objects;

/**
 * The special forms contains a list of expressions (the symbol is not part of the list).
 * <p>
 *     Special forms are forms which are the bare minimum to form a turin complete language.
 * </p>
 */
public abstract class SlartiSpecialForm extends SlartiList {

    private final SlartiSymbol symbol;

    /**
     * Dedicated constructor.
     *
     * @param symbol must not be {@code null}
     * @param list must not be {@code null}
     */
    SlartiSpecialForm(final SlartiSymbol symbol, final SlartiList list) {
        super(Validate.notNull(list, "list").data());
        this.symbol = Validate.notNull(symbol, "symbol");
    }

    /**
     * Determine sif the head of the givne list is a special form symbol.
     * <p>
     *     If the given lists head is a special form it is substituted by the special form.
     *     All other lists are returned untouched.
     * </p>
     * @param list must not be {@code null}
     * @return never {@code null}
     */
    public static SlartiNode check(final SlartiList list) {
        Validate.notNull(list, "list");

        if (SlartiList.EMPTY.equals(list)) {
            return list;
        }

        final SlartiNode head = list.head();

        if (DefineSpecialForm.SYMBOL.equals(head)) {
            return new DefineSpecialForm(list.tail());
        } else if (LambdaSpecialForm.SYMBOL.equals(head)) {
            return new LambdaSpecialForm(list.tail());
        } else if (IfSpecialForm.SYMBOL.equals(head)) {
            return new IfSpecialForm(list.tail());
        } else if (QuoteSpecialForm.SYMBOL.equals(head) || QuoteSpecialForm.ALIAS.equals(head)) {
            return new QuoteSpecialForm(list.tail());
        } else {
            return list;
        }
    }

    /**
     * Name symbol of the special form.
     *
     * @return never {@code null}
     */
    final SlartiSymbol symbol() {
        return symbol;
    }

    @Override
    public final boolean equals(final Object o) {
        if (!(o instanceof SlartiSpecialForm)) {
            return false;
        }

        final SlartiSpecialForm that = (SlartiSpecialForm) o;
        return that.canEqual(this) &&
            Objects.equals(symbol, that.symbol) &&
            super.equals(that);
    }

    @Override
    public final boolean canEqual(final Object other) {
        return other instanceof SlartiSpecialForm;
    }

    @Override
    public final int hashCode() {
        return Objects.hash(symbol, super.hashCode());
    }

    @Override
    public final String toString() {
        return '(' + symbol().name() + ' ' + data().itemsAsString() + ')';
    }
}
