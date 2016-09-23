package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.node.*;
import de.weltraumschaf.slartibartfass.node.function.SlartiFunction;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Stream;

/**
 * This special form creates a lambda function.
 * <p>
 *     Syntax: {@code (lambda (x) (* x x))}.
 * </p>
 * <p>
 *     The list given to the constructor contains the formal arguments as first element and the body o the lambda function
 *     as second element. The {@link #eval(Environment) evaluation} generates the {@link SlartiFunction function} for this
 *     arguments on the fly. Use this special form in conjunction with the {@link DefineSpecialForm define special form}
 *     to store the function under a name in the environment.
 * </p>
 */
public final class LambdaSpecialForm extends SlartiSpecialForm {

    static final SlartiSymbol SYMBOL = new SlartiSymbol("lambda");

    /**
     * Dedicated constructor.
     *
     * @param list must not be {@code null}
     */
    public LambdaSpecialForm(final SlartiList list) {
        super(SYMBOL, list);
    }

    @Override
    public SlartiNode eval(final Environment env) {
        final SlartiList formalParams = (SlartiList) head();
        final SlartiList functionBody = (SlartiList) tail().head();

        return SlartiFunction.newFunction(env, symbol().name(), formalParams, functionBody);
    }

}
