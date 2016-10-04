package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.node.SlartiNode;

/**
 * Base for all built in functions.
 *
 * @author Sven Strittmatter
 */
abstract class SlartiBuiltInFunctionBase extends SlartiFunction {
    /**
     * Used for I/O if the function need that.
     */
    protected IO io;

    /**
     * Dedicated constructor.
     *
     * @param name must not be {@code null} or empty
     */
    SlartiBuiltInFunctionBase(String name) {
        super(name);
    }

    @Override
    public final boolean isBuiltIn() {
        return true;
    }

    /**
     * Injection point for the I/O.
     *
     * @param io must not be {@code null} or empty
     */
    protected void setIo(final IO io) {
        this.io = Validate.notNull(io, "io");
    }

    /**
     * Used to create an runtime error if an argument has the wrong type.
     *
     * @param argument must not be {@code null}
     * @return never {@code null}
     */
    SlartiError unsupportedTypeOfArgument(final SlartiNode<?> argument) {
        return new SlartiError(
            String.format("Unsupported type %s of argument for function %s!",
                argument.getClass().getSimpleName(), symbol()));
    }

    void errorIfNotNumber(final SlartiNode arg) {
        if (!arg.isInteger() && !arg.isReal()) {
            throw unsupportedTypeOfArgument(arg);
        }
    }

    boolean isZero(final SlartiNode arg) {
        if (arg.isInteger() && arg.castToInteger().value() == 0L) {
            return true;
        }

        if (arg.isReal() && arg.castToReal().value() == 0d) {
            return true;
        }

        return false;
    }
}
