package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.SlartiList;
import de.weltraumschaf.slartibartfass.node.SlartiSymbol;

/**
 * Syntax: {@code (define name (VALUE) )}.
 */
public final class DefineSpecialForm extends SlartiSpecialForm {

    public DefineSpecialForm(final SlartiList list) {
        super(DEFINE, list);
    }

    @Override
    public Object eval(final Environment env) {
        final SlartiSymbol sym = (SlartiSymbol) head();
        env.putValue(sym.name(), tail().head().eval(env));
        return SlartiList.EMPTY;
    }

}
