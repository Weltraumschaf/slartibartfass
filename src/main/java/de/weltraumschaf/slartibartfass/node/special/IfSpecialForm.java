package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

/**
 * Syntax: {@code (if (CONDITION) (THEN) (ELSE) )}.
 */
public final class IfSpecialForm extends SlartiSpecialForm {

    public IfSpecialForm(final SlartiList list) {
        super(IF, list);
    }

    @Override
    public Object eval(final Environment env) {
        final SlartiNode condition = head();

        if (Boolean.parseBoolean(condition.eval(env).toString())) {
            final SlartiNode thenBranch = tail().head();
            return thenBranch.eval(env);
        }

        final SlartiNode elseBranch = tail().tail().head();
        return elseBranch.eval(env);
    }
}
