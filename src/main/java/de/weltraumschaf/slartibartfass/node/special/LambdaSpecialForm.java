package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.*;
import de.weltraumschaf.slartibartfass.node.function.SlartiFunction;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

import java.util.Collection;
import java.util.List;

/**
 * Syntax: {@code (lambda (x) (* x x))}.
 */
public final class LambdaSpecialForm extends SlartiSpecialForm {

    public LambdaSpecialForm(final SlartiList list) {
        super(LAMBDA, list);
    }

    @Override
    public Object eval(final Environment parentEnv) {
        final SlartiList formalParams = (SlartiList) head();
        final SlartiList functionBody = (SlartiList) tail().head();

        return new SlartiFunction(LAMBDA.name()) {

            @Override
            public Object apply(final List<Object> actualParameters) {
                validateParameterCount(actualParameters);
                final Environment localScope = new Environment(parentEnv);
                mapParametersIntoLocalScope(actualParameters, localScope);

                return functionBody.eval(localScope);
            }

            @Override
            public boolean isBuiltIn() {
                return false;
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
            public String toString() {
                return '(' + super.toString() + ' ' + formalParams.toString() + ' ' + functionBody.toString() + ')';
            }
        };
    }
}
