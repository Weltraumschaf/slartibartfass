package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

public final class QuoteSpecialForm extends SlartiSpecialForm {

    static final SlartiSymbol SYMBOL = new SlartiSymbol("quote");
    static final SlartiSymbol ALIAS = new SlartiSymbol("'");

    public QuoteSpecialForm(final SlartiList list) {
        super(SYMBOL, list);
    }

    @Override
    public Object eval(final Environment env) {
        final StringBuilder buffer = new StringBuilder();
        String sep = "";

        for (final SlartiNode node : data()) {
            buffer.append(sep).append(node.toString());
            sep = " ";
        }

        return buffer.toString();
    }
}
