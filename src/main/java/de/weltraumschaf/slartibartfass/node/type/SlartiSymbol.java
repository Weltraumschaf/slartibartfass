package de.weltraumschaf.slartibartfass.node.type;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.backend.Environment;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

import java.util.Objects;

/**
 * Symbol type of the language.
 * <p>
 * Symbols are used to symbol things. This is used together with the {@link de.weltraumschaf.slartibartfass.node.special.DefineSpecialForm
 * define special form} to add variables into the {@link Environment}.
 * </p>
 * <p>
 * {@link #eval(Environment) Evaluating} this node will look in the given {@link Environment environment} and all
 * its parent for the symbol and returns the value of the first match.
 * </p>
 *
 * @author Sven Strittmatter
 */
public final class SlartiSymbol implements SlartiNode<String> {

    /**
     * Literal symbol symbol.
     */
    private final String name;

    /**
     * Dedicated constructor.
     *
     * @param name must not be {@code null} or empty
     */
    public SlartiSymbol(final String name) {
        super();
        this.name = Validate.notEmpty(name, "symbol");
    }

    /**
     * The symbol of the symbol.
     *
     * @return never {@code null} or empty
     */
    public String name() {
        return name;
    }

    @Override
    public SlartiNode eval(final Environment env) {
        return env.getValue(this).memory();
    }

    @Override
    public boolean equals(final Object o) {
        if (!(o instanceof SlartiSymbol)) {
            return false;
        }

        final SlartiSymbol that = (SlartiSymbol) o;
        return Objects.equals(name, that.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public String value() {
        return name();
    }

    @Override
    public SlartiBoolean castToBoolean() {
        throw unsupportedCastError(SlartiBoolean.class);
    }

    @Override
    public SlartiInteger castToInteger() {
        throw unsupportedCastError(SlartiInteger.class);
    }

    @Override
    public SlartiReal castToReal() {
        throw unsupportedCastError(SlartiReal.class);
    }

    @Override
    public SlartiString castToString() {
        return new SlartiString(name());
    }

    @Override
    public SlartiList castToList() {
        return new SlartiList(this);
    }

}
