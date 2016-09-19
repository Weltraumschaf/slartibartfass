package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.*;
import de.weltraumschaf.slartibartfass.node.function.SlartiFunction;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

import java.util.Collection;
import java.util.List;

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
    public Object eval(final Environment env) {
        final SlartiList formalParams = (SlartiList) head();
        final SlartiList functionBody = (SlartiList) tail().head();

        return createFunction(env, symbol().name(), formalParams, functionBody);
    }

    private SlartiFunction createFunction(final Environment parentEnv, final String name, final SlartiList formalParams, final SlartiList functionBody) {
        return new SlartiFunction(name) {

            @Override
            public final Object apply(final List<Object> actualParameters) {
                validateParameterCount(actualParameters);
                final Environment localScope = new Environment(parentEnv);
                mapParametersIntoLocalScope(actualParameters, localScope);

                return functionBody.eval(localScope);
            }

            private void mapParametersIntoLocalScope(List<Object> args, Environment localScope) {
                int i = 0;
                for (final SlartiNode param : formalParams) {
                    final SlartiSymbol paramSymbol = (SlartiSymbol) param;
                    localScope.putValue(paramSymbol.name(), args.get(i));
                    i++;
                }
            }

            private void validateParameterCount(final Collection<Object> args) {
                if (args.size() != formalParams.size()) {
                    throw new RuntimeException(String.format(
                        "Wrong number of arguments. Expected: %d. Got: %d!",
                        formalParams.size(), args.size()));
                }
            }

            @Override
            public final boolean isBuiltIn() {
                return false;
            }

            @Override
            public final String toString() {
                return '(' + super.toString() + ' ' + formalParams.toString() + ' ' + functionBody.toString() + ')';
            }
        };
    }
}
