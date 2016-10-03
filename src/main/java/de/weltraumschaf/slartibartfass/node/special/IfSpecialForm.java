package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

/**
 * THis special form branches the execution based on the boolean result of a condition expression.
 * <p>
 *     Syntax: {@code (if (CONDITION) (THEN) (ELSE))} or without else branch {@code (if (CONDITION) (THEN))}.
 * </p>
 * <p>
 *     If the else branch is omitted and the condition evaluates to false an empty list will be returned.
 * </p>
 *
 * @author Sven Strittmatter
 */
public final class IfSpecialForm extends SlartiSpecialForm {

    /**
     * Symbol of the special form.
     */
    static final SlartiSymbol SYMBOL = new SlartiSymbol("if");

    /**
     * Dedicated constructor.
     *
     * @param arguments must not be {@code null}
     */
    public IfSpecialForm(final SlartiList arguments) {
        super(SYMBOL, arguments);
    }

    @Override
    public SlartiNode eval(final Environment env) {
        final SlartiNode condition = head();

        if (size() < 2) {
            throw new SlartiError("The if special form requires at least a condition and a then expression!");
        }

        if (size() > 3) {
            throw new SlartiError("The if special form requires maximum a condition, then and else expression!");
        }

        if (condition.eval(env).castToBoolean().value()) {
            final SlartiNode thenBranch = tail().head();
            return thenBranch.eval(env);
        }

        if (size() < 3) {
            // There is no else branch.
            return SlartiList.NIL;
        }

        final SlartiNode elseBranch = tail().tail().head();
        return elseBranch.eval(env);
    }
}
