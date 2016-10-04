package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.function.SlartiFunction;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

/**
 * This special form allocates memory either for variables or functions.
 * <p>
 * Syntax: {@code (define var-symbol (VALUE))} or {@code (define (function-symbol <formal-params>) <function-body>)}.
 * </p>
 *
 * @author Sven Strittmatter
 */
public final class DefineSpecialForm extends SlartiSpecialForm {

    /**
     * Symbol of the special form.
     */
    static final SlartiSymbol SYMBOL = new SlartiSymbol("define");

    /**
     * Dedicated constructor.
     *
     * @param arguments must not be {@code null}
     */
    public DefineSpecialForm(final SlartiList arguments) {
        super(SYMBOL, arguments);
    }

    @Override
    public SlartiNode eval(final Environment env) {
        final SlartiNode head = head();

        if (head.isSymbol()) {
            return defineSymbol(env, (SlartiSymbol) head);
        } else if (head.isList()) {
            return defineFunction(env, head);
        } else {
            throw new SlartiError("Unsupported value as first argument of define special form: %s!", head);
        }
    }

    private SlartiNode defineSymbol(final Environment env, final SlartiSymbol name) {
        env.putValue(name, tail().head().eval(env));
        return name;
    }

    private SlartiNode defineFunction(final Environment env, final SlartiNode head) {
        final SlartiList list = head.castToList();

        if (!list.head().isSymbol()) {
            throw new SlartiError("Symbol expected of first list parameter! Got %s.", list.head());
        }

        final SlartiSymbol name = (SlartiSymbol) list.head();
        final SlartiList formalParams = list.tail();
        final SlartiList functionBody = tail();

        env.putValue(name, SlartiFunction.newFunction(env, name, formalParams, functionBody));
        return name;
    }

}
