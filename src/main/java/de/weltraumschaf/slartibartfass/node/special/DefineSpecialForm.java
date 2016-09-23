package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.function.SlartiFunction;
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
        final SlartiNode head = head();

        if (head.isSymbol()) {
            final SlartiSymbol name = (SlartiSymbol) head;
            env.putValue(name.name(), tail().head().eval(env));
        } else if (head.isList()){
            final SlartiList list = head.castToList();

            if (!list.head().isSymbol()) {
                throw new SlartiError("Symbol expected of first list parameter! Got %s.", list.head());
            }

            final SlartiSymbol name = (SlartiSymbol) list.head();
            final SlartiList formalParams = list.tail();
            final SlartiList functionBody = tail();

            env.putValue(name.name(), SlartiFunction.newFunction(env, name, formalParams, functionBody));
        } else {
            throw new SlartiError("Unsupported value as first argument of define special form: %s!", head);
        }

        return SlartiList.EMPTY;
    }

}
