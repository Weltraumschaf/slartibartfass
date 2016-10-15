package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.backend.Environment;
import de.weltraumschaf.slartibartfass.node.*;
import de.weltraumschaf.slartibartfass.node.function.SlartiFunction;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

/**
 * This special form creates a lambda function.
 * <p>
 * Syntax: {@code (lambda (x) (* x x))}.
 * </p>
 * <p>
 * The list given to the constructor contains the formal arguments as first element and the body o the lambda function
 * as second element. The {@link #eval(Environment) evaluation} generates the {@link SlartiFunction function} for this
 * arguments on the fly. Use this special form in conjunction with the {@link DefineSpecialForm define special form}
 * to store the function under a name in the environment.
 * </p>
 *
 * @author Sven Strittmatter
 */
public final class LambdaSpecialForm extends SlartiSpecialForm {

    /**
     * Symbol of the special form.
     */
    static final SlartiSymbol SYMBOL = new SlartiSymbol("lambda");

    /**
     * Dedicated constructor.
     *
     * @param arguments must not be {@code null}
     */
    public LambdaSpecialForm(final SlartiList arguments) {
        super(SYMBOL, arguments);
    }

    @Override
    public SlartiNode eval(final Environment env) {
        final SlartiList formalParams = head().castToList();
        final SlartiList functionBody = tail().head().castToList();

        return SlartiFunction.newFunction(env, symbol(), formalParams, functionBody);
    }

}
