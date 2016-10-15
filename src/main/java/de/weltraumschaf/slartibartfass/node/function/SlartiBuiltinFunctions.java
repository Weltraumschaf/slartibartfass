package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.backend.Environment;
import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiBoolean;
import de.weltraumschaf.slartibartfass.node.type.SlartiInteger;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;

import java.util.List;
import java.util.Random;
import static de.weltraumschaf.slartibartfass.node.Slarti.*;

/**
 * All provided built in functions.
 *
 * TODO list, head, tail functions.
 *
 * @author Sven Strittmatter
 */
public enum SlartiBuiltinFunctions {
    /**
     * Number addition.
     */
    ADD(new SlartiBuiltInFunctionBase("+") {


        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            final ResultAccumulator accu = new ResultAccumulator(
                (l, r) -> l + r,
                (l, r) -> l + r
            );

            for (final SlartiNode arg : args) {
                errorIfNotNumber(arg);
                accu.apply(arg);
            }

            return accu.result();
        }
    }),
    /**
     * Number subtraction.
     */
    SUBTRACT(new SlartiBuiltInFunctionBase("-") {
        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            if (args.size() < 1) {
                throw new SlartiError("Function %s requires at least one argument!", symbol());
            } else if (args.size() == 1) {
                final SlartiNode arg = args.get(0);
                errorIfNotNumber(arg);

                if  (arg.isInteger()) {
                    return of(- arg.castToInteger().value());
                } else if (arg.isReal()) {
                    return of(- arg.castToReal().value());
                }

                return null; // Never eached because error will be thrown if wrong bd above.
            } else {
                final ResultAccumulator accu = new ResultAccumulator(
                    (l, r) -> l - r,
                    (l, r) -> l - r
                );

                accu.init(args.get(0));

                for (int i = 1; i < args.size(); ++i) {
                    final SlartiNode arg = args.get(i);
                    errorIfNotNumber(arg);
                    accu.apply(arg);
                }

                return accu.result();
            }
        }
    }),
    /**
     * Number multiplication.
     */
    MULTIPLY(new SlartiBuiltInFunctionBase("*") {
        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            final ResultAccumulator accu = new ResultAccumulator(
                (l, r) -> l * r,
                (l, r) -> l * r
            );
            accu.init(of(1L));

            for (final SlartiNode arg : args) {
                errorIfNotNumber(arg);
                accu.apply(arg);
            }

            return accu.result();
        }
    }),
    /**
     * Number division.
     */
    DIVISION(new SlartiBuiltInFunctionBase("/") {
        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            if (args.size() < 2) {
                throw new SlartiError("Function %s requires at least two arguments!", symbol());
            }

            final ResultAccumulator accu = new ResultAccumulator(
                (l, r) -> l / r,
                (l, r) -> l / r
            );
            accu.init(args.get(0));

            for (final SlartiNode arg : args.subList(1, args.size())) {
                errorIfNotNumber(arg);

                if (isZero(arg)) {
                    throw new SlartiError("Division by zero!");
                }

                accu.apply(arg);
            }

            return accu.result();
        }
    }),
    /**
     * Integer modulo.
     */
    MODULO(new SlartiBuiltInFunctionBase("%") {
        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            if (args.size() < 2) {
                throw new SlartiError("Function %s requires at least two arguments!", symbol());
            }

            final ResultAccumulator accu = new ResultAccumulator(
                (l, r) -> l % r,
                (l, r) -> l % r
            );
            accu.init(args.get(0));

            for (final SlartiNode arg : args.subList(1, args.size())) {
                errorIfNotNumber(arg);

                if (isZero(arg)) {
                    throw new SlartiError("Division by zero!");
                }

                accu.apply(arg);
            }

            return accu.result();
        }
    }),
    /**
     * Less than number comparison.
     */
    LESS_THAN(new SlartiBuiltInFunctionBase("<") {
        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            if (args.size() != 2) {
                throw new SlartiError("Function %s requires two arguments!", symbol());
            }

            final SlartiNode left = args.get(0);
            errorIfNotNumber(left);
            final SlartiNode right = args.get(1);
            errorIfNotNumber(right);

            // Cast to real so also integers are covered.
            return left.castToReal().value() < right.castToReal().value() ?
                SlartiBoolean.TRUE :
                SlartiBoolean.FALSE;
        }
    }),
    /**
     * Greater than number comparison.
     */
    GREATER_THAN(new SlartiBuiltInFunctionBase(">") {
        @Override
        public SlartiNode apply(List<SlartiNode> args) {
            if (args.size() != 2) {
                throw new SlartiError("Function %s requires two arguments!", symbol());
            }

            final SlartiNode left = args.get(0);
            errorIfNotNumber(left);
            final SlartiNode right = args.get(1);
            errorIfNotNumber(right);

            // Cast to real so also integers are covered.
            return left.castToReal().value() > right.castToReal().value() ?
                SlartiBoolean.TRUE :
                SlartiBoolean.FALSE;
        }
    }),
    /**
     * Equals comparison.
     */
    EQUALS(new SlartiBuiltInFunctionBase("=") {
        @Override
        public SlartiNode apply(List<SlartiNode> args) {
            if (args.size() != 2) {
                throw new SlartiError("Function %s requires two arguments!", symbol());
            }

            final SlartiNode left = args.get(0);
            final SlartiNode right = args.get(1).castTo(left);

            return left.equals(right) ? SlartiBoolean.TRUE : SlartiBoolean.FALSE;
        }
    }),
    /**
     * Logical and of booleans.
     */
    AND(new SlartiBuiltInFunctionBase("and") {
        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            if (args == null || args.isEmpty()) {
                return SlartiBoolean.FALSE;
            }

            for (final SlartiNode arg : args) {
                if (!arg.castToBoolean().value()) {
                    return SlartiBoolean.FALSE;
                }
            }

            return SlartiBoolean.TRUE;
        }
    }),
    /**
     * Logical or of booleans.
     */
    OR(new SlartiBuiltInFunctionBase("or") {
        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            if (args == null || args.isEmpty()) {
                return SlartiBoolean.FALSE;
            }

            for (final SlartiNode arg : args) {
                if (arg.castToBoolean().value()) {
                    return SlartiBoolean.TRUE;
                }
            }

            return SlartiBoolean.FALSE;
        }
    }),
    /**
     * Prints the given arguments appended with a newline.
     */
    PRINTLN(new SlartiBuiltInFunctionBase("println") {
        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            final StringBuilder buffer = new StringBuilder();

            for (final SlartiNode arg : args) {
                buffer.append(arg.castToString().value());
            }

            io.println(buffer.toString());
            return SlartiList.NIL;
        }
    }),
    /**
     * Prints the given arguments.
     */
    PRINT(new SlartiBuiltInFunctionBase("print") {
        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            final StringBuilder buffer = new StringBuilder();

            for (final SlartiNode arg : args) {
                buffer.append(arg.castToString().value());
            }

            io.print(buffer.toString());
            return SlartiList.NIL;
        }
    }),
    /**
     * Generates a random integer.
     */
    RANDOM(new SlartiBuiltInFunctionBase("random") {
        private final Random r = new Random();

        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            return new SlartiInteger(r.nextLong());
        }
    }),
    /**
     * Function to create lists.
     */
    LIST(new SlartiBuiltInFunctionBase("list") {

        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            return new SlartiList(args);
        }
    }),
    /**
     * Function to get the head of a list.
     */
    HEAD(new SlartiBuiltInFunctionBase("head") {

        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            if (args.size() != 1) {
                throw new SlartiError("Function %s expects exactly one argument!");
            }

            final SlartiNode list = args.get(0);

            if (!list.isList()) {
                throw new SlartiError("Function %s expects a list as argument!");
            }

            if (list.castToList().isEmpty()) {
                return SlartiBoolean.FALSE;
            }

            return list.castToList().head();
        }
    }),
    /**
     * Function to get the tail of a list.
     */
    TAIL(new SlartiBuiltInFunctionBase("tail") {

        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            if (args.size() != 1) {
                throw new SlartiError("Function %s expects exactly one argument!");
            }

            final SlartiNode list = args.get(0);

            if (!list.isList()) {
                throw new SlartiError("Function %s expects a list as argument!");
            }

            return list.castToList().tail();
        }
    });

    /**
     * Holds the implementation of the built in function.
     */
    private final SlartiFunction impl;

    /**
     * Dedicated constructor.
     *
     * @param impl must not be {@code null}
     */
    SlartiBuiltinFunctions(final SlartiFunction impl) {
        this.impl = Validate.notNull(impl, "impl");
    }

    /**
     * Returns the implementation of the built in function.
     *
     * @return never {@code null} always same instance
     */
    public SlartiFunction impl() {
        return impl;
    }

    /**
     * Register all built in functions in the given environment.
     *
     * @param env must not be {@code null}
     * @param io must not be {@code null}
     */
    public static void register(final Environment env, final IO io) {
        Validate.notNull(env, "env");

        for (final SlartiBuiltinFunctions fn : values()) {
            ((SlartiBuiltInFunctionBase) fn.impl()).setIo(io);
            env.putValue(fn.impl.symbol(), fn.impl);
        }
    }

}
