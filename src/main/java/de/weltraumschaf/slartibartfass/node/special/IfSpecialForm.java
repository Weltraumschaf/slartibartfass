package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

/**
 * Syntax: {@code (if (CONDITION) (THEN) (ELSE) )}.
 */
public final class IfSpecialForm extends SlartiSpecialForm {

    static final SlartiSymbol SYMBOL = new SlartiSymbol("if");

    public IfSpecialForm(final SlartiList list) {
        super(SYMBOL, list);
    }

    @Override
    public SlartiNode eval(final Environment env) {
        final SlartiNode condition = head();

        if (condition.eval(env).castToBoolean().value()) {
            final SlartiNode thenBranch = tail().head();
            return thenBranch.eval(env);
        }

        final SlartiNode elseBranch = tail().tail().head();
        return elseBranch.eval(env);
    }
}
