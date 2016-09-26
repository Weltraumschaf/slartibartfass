package de.weltraumschaf.slartibartfass.node.special;

import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.InternalList;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiString;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

import java.util.ArrayList;
import java.util.List;

/**
 * This special form treats the quoted argument literal instead of evaluate it.
 * <p>
 *     Syntax: {@code (quote foo)} or {@code 'foo}
 * </p>
 * <p>
 *     Usually expressions are evaluated directly:
 * </p>
 * <pre>
 *     sl> (define a 2)
 *     sl> (define b 3)
 *     sl> (list a b)
 *     (2 3)
 * </pre>
 * <p>
 *     With quote the quoted expressions ae not evaluated:
 * </p>
 * <pre>
 *     sl> (define a 2)
 *     sl> (define b 3)
 *     sl> (list 'a 'b)
 *     (a b)
 * </pre>
 *
 * @author Sven Strittmatter
 */
public final class QuoteSpecialForm extends SlartiSpecialForm {

    /**
     * Symbol of the special form.
     */
    static final SlartiSymbol SYMBOL = new SlartiSymbol("quote");

    /**
     * Dedicated constructor.
     *
     * @param arguments must not be {@code null}
     */
    public QuoteSpecialForm(final SlartiList arguments) {
        super(SYMBOL, arguments);
    }

    @Override
    public SlartiNode eval(final Environment env) {
        if (size() == 1) {
            return head();
        }

        final List<SlartiNode> quoted = new ArrayList<>();

        for (final SlartiNode node : data()) {
            quoted.add(node);
        }

        return new SlartiList(quoted);
    }
}
