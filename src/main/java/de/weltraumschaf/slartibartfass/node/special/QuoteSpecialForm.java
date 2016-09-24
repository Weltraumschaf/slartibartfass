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
    static final SlartiSymbol ALIAS = new SlartiSymbol("'");

    public QuoteSpecialForm(final SlartiList list) {
        super(SYMBOL, list);
    }

    @Override
    public SlartiNode eval(final Environment env) {
        final List<SlartiNode> quoted = new ArrayList<>();

        for (final SlartiNode node : data()) {
            quoted.add(node);
        }

        return new SlartiList(quoted);
    }
}
