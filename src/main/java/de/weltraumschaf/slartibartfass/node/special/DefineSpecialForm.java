package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

/**
 * Syntax: {@code (define symbol (VALUE) )}.
 */
public final class DefineSpecialForm extends SlartiSpecialForm {

    static final SlartiSymbol SYMBOL = new SlartiSymbol("define");

    public DefineSpecialForm(final SlartiList list) {
        super(SYMBOL, list);
    }

    @Override
    public SlartiNode eval(final Environment env) {
        final SlartiSymbol sym = (SlartiSymbol) head();
        env.putValue(sym.name(), tail().head().eval(env));
        return SlartiList.EMPTY;
    }

}
