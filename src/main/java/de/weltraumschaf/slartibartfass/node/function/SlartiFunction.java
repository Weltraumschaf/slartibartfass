package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.type.SlartiSymbol;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

import static de.weltraumschaf.slartibartfass.node.Slarti.sym;

/**
 * This is the base type for function invocations.
 * <p>
 * {@link #eval(Environment) Evaluating} this node will return the node itself.
 * </p>
 *
 * @author Sven Strittmatter
 */
public abstract class SlartiFunction implements SlartiNode {
    /**
     * The symbol of the function.
     */
    private final SlartiSymbol symbol;

    /**
     * Convenience constructor.
     *
     * @param symbol must not be {@code null} or empty
     */
    protected SlartiFunction(final String symbol) {
        this(sym(symbol));
    }

    /**
     * Dedicated constructor.
     *
     * @param symbol must not be {@code null} or empty
     */
    protected SlartiFunction(final SlartiSymbol symbol) {
        super();
        this.symbol = Validate.notNull(symbol, "symbol");
    }

    @Override
    public final SlartiNode eval(final Environment env) {
        return this;
    }

    /**
     * Convenience method.
     *
     * @param args must not be {@code null}
     * @return never {@code null}
     * @see #apply(List)
     */
    public final SlartiNode apply(final SlartiNode... args) {
        return apply(Arrays.asList(args));
    }

    /**
     * Applies the function for the given arguments.
     *
     * @param args must not be {@code null}
     * @return never {@code null}
     */
    public abstract SlartiNode apply(List<SlartiNode> args);

    /**
     * Whether the function is built in or user defined.
     *
     * @return {@code true} if built in, {@code false} for user defined
     */
    public abstract boolean isBuiltIn();

    /**
     * Get the literal symbol of the function.
     *
     * @return never {@code null} or empty
     */
    public final SlartiSymbol symbol() {
        return symbol;
    }

    @Override
    public final boolean equals(final Object o) {
        if (!(o instanceof SlartiFunction)) {
            return false;
        }

        final SlartiFunction that = (SlartiFunction) o;
        return Objects.equals(symbol, that.symbol);
    }

    @Override
    public final int hashCode() {
        return Objects.hash(symbol);
    }

    @Override
    public String toString() {
        return symbol.name();
    }

    /**
     * Factory method to create anonymous functions.
     *
     * @param parentEnv    enclosing scope, must not be {@code null}
     * @param name         function symbol ,must not be {@code null} or empty
     * @param formalParams must not be {@code null}
     * @param functionBody must not be {@code null}
     * @return never {@code null}
     */
    public static SlartiFunction newFunction(final Environment parentEnv, final SlartiSymbol name, final SlartiList formalParams, final SlartiList functionBody) {
        return new SlartiFunction(name.name()) {

            @Override
            public SlartiNode apply(final List<SlartiNode> actualParameters) {
                validateParameterCount(actualParameters);
                final Environment localScope = new Environment(parentEnv);
                mapParametersIntoLocalScope(actualParameters, localScope);

                return functionBody.eval(localScope);
            }

            private void mapParametersIntoLocalScope(final List<SlartiNode> args, final Environment localScope) {
                int i = 0;
                for (final SlartiNode param : formalParams) {
                    final SlartiSymbol paramSymbol = (SlartiSymbol) param;
                    localScope.putValue(paramSymbol, args.get(i));
                    i++;
                }
            }

            private void validateParameterCount(final Collection<SlartiNode> args) {
                if (args.size() != formalParams.size()) {
                    throw new SlartiError(
                        "Wrong number of arguments. Expected: %d. Got: %d!",
                        formalParams.size(), args.size());
                }
            }

            @Override
            public boolean isBuiltIn() {
                return false;
            }

            @Override
            public String toString() {
                return '(' + super.toString() + ' ' + formalParams.toString() + ' ' + functionBody.toString() + ')';
            }
        };
    }
}
