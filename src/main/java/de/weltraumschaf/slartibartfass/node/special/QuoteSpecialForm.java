package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

public final class QuoteSpecialForm extends SlartiSpecialForm {

    public QuoteSpecialForm(final SlartiList list) {
        super(QUOTE, list);
    }

    @Override
    public Object eval(final Environment env) {
        final StringBuilder buffer = new StringBuilder();
        String sep = "";

        for (final SlartiNode node : data) {
            buffer.append(sep).append(node.toString());
            sep = " ";
        }

        return buffer.toString();
    }
}
