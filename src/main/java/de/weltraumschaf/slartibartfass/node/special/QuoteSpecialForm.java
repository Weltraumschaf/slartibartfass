package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.InternalList;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiString;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

import java.util.ArrayList;
import java.util.List;

public final class QuoteSpecialForm extends SlartiSpecialForm {

    static final SlartiSymbol SYMBOL = new SlartiSymbol("quote");

    public QuoteSpecialForm(final SlartiList list) {
        super(SYMBOL, list);
    }

    @Override
    public SlartiNode eval(final Environment env) {
        if (size() == 1) {
            return head();
        }

        final List<SlartiNode> quoted = new ArrayList<>();

        for (final SlartiNode node : data()) {
            quoted.add(node);
        }

        return new SlartiList(quoted);
    }
}
