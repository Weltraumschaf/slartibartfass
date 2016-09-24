package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiBoolean;
import de.weltraumschaf.slartibartfass.node.type.SlartiInteger;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;
import de.weltraumschaf.slartibartfass.node.type.SlartiReal;
import jdk.nashorn.internal.objects.NativeDebug;

import java.util.List;
import java.util.Random;

/**
 * All provided built in functions.
 */
public enum SlartiBuiltinFunctions {
    /**
     * Number addition.
     */
    ADD(new SlartiBuiltInFunctio("+") {
        @Override
        public final SlartiNode apply(final List<SlartiNode> args) {
            long sum = 0;

            for (final SlartiNode arg : args) {
                if  (arg.isInteger()) {
                    sum += arg.castToInteger().value();
                } else if (arg.isReal()) {
                    // FIXME Deal with real numbers.
                    throw new UnsupportedOperationException("Not implemented!");
                } else {
                    throw unsupportedTypeOfArgument(arg);
                }
            }

            return new SlartiInteger(sum);
        }
    }),
    /**
     * Number subtraction.
     */
    SUBTRACT(new SlartiBuiltInFunctio("-") {
        @Override
        public final SlartiNode apply(final List<SlartiNode> args) {
            if (args.size() < 1) {
                throw new SlartiError("Function %s requires at least one argument!", name());
            } else if (args.size() == 1) {
                final SlartiNode arg = args.get(0);

                if  (arg.isInteger()) {
                    return new SlartiInteger(- arg.castToInteger().value());
                } else if (arg.isReal()) {
                    return new SlartiReal(- arg.castToReal().value());
                } else {
                    throw unsupportedTypeOfArgument(arg);
                }
            } else {
                long intResult = 0L;
                double realResult = 0d;
                boolean realSeen = false;

                final SlartiNode first = args.get(0);

                if (first.isInteger()) {
                    intResult = first.castToInteger().value();
                } else if (first.isReal()) {
                    realResult = first.castToReal().value();
                    realSeen = true;
                } else {
                    throw unsupportedTypeOfArgument(first);
                }


                for (int i = 1; i < args.size(); i++) {
                    final SlartiNode arg = args.get(i);

                    if (arg.isInteger()) {
                        if (realSeen) {
                            realResult -= arg.castToInteger().value().doubleValue();
                        } else {
                            intResult -= arg.castToInteger().value();
                        }
                    } else if (arg.isReal()) {
                        if (realSeen) {
                            realResult -= arg.castToReal().value();
                        } else {
                            realResult = intResult - arg.castToReal().value();
                            realSeen = true;
                        }
                    } else {
                        throw unsupportedTypeOfArgument(first);
                    }
                }

                return realSeen ? new SlartiReal(realResult) : new SlartiInteger(intResult);
            }
        }
    }),
    /**
     * Number multiplication.
     */
    MULTIPLY(new SlartiBuiltInFunctio("*") {
        @Override
        public final SlartiNode apply(final List<SlartiNode> args) {
            // FIXME Deal with real numbers.
            long result = 1;

            for (final SlartiNode arg : args) {
                result *= Long.parseLong(arg.toString());
            }

            return new SlartiInteger(Long.valueOf(result));
        }
    }),
    /**
     * Number division.
     */
    DIVISION(new SlartiBuiltInFunctio("/") {
        @Override
        public final SlartiNode apply(final List<SlartiNode> args) {
            // FIXME Deal with real numbers.
            // FIXME Handle division by 0.
            if (args.size() < 2) {
                throw new SlartiError("Function %s requires at least two arguments!", name());
            }

            long result = Long.parseLong(args.get(0).toString());

            for (final SlartiNode arg : args.subList(1, args.size())) {
                result /= Long.parseLong(arg.toString());
            }

            return new SlartiInteger(result);
        }
    }),
    /**
     * Integer modulo.
     */
    MODULO(new SlartiBuiltInFunctio("%") {
        @Override
        public final SlartiNode apply(final List<SlartiNode> args) {
            // FIXME Deal with real numbers.
            // FIXME Handle division by 0.
            if (args.size() < 2) {
                throw new SlartiError("Function %s requires at least two arguments!", name());
            }

            long result = Long.parseLong(args.get(0).toString());

            for (final SlartiNode arg : args.subList(1, args.size())) {
                result %= Long.parseLong(arg.toString());
            }

            return new SlartiInteger(result);
        }
    }),
    /**
     * Less than number comparison.
     */
    LESS_THAN(new SlartiBuiltInFunctio("<") {
        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            // FIXME Deal with real numbers.
            if (args.size() != 2) {
                throw new SlartiError("Function %s requires two arguments!", name());
            }

            final long left = Long.parseLong(args.get(0).toString());
            final long right = Long.parseLong(args.get(1).toString());

            return left < right ? SlartiBoolean.TRUE : SlartiBoolean.FALSE;
        }
    }),
    /**
     * Greater than number comparison.
     */
    GREATER_THAN(new SlartiBuiltInFunctio(">") {
        // FIXME Deal with real numbers.
        @Override
        public SlartiNode apply(List<SlartiNode> args) {
            if (args.size() != 2) {
                throw new SlartiError("Function %s requires two arguments!", name());
            }

            final long left = Long.parseLong(args.get(0).toString());
            final long right = Long.parseLong(args.get(1).toString());

            return left > right? SlartiBoolean.TRUE : SlartiBoolean.FALSE;
        }
    }),
    /**
     * Equals comparison.
     */
    EQUALS(new SlartiBuiltInFunctio("=") {
        @Override
        public SlartiNode apply(List<SlartiNode> args) {
            // FIXME Deal with real numbers.
            if (args.size() != 2) {
                throw new SlartiError("Function %s requires two arguments!", name());
            }

            final SlartiNode left = args.get(0);
            final SlartiNode right = args.get(1);

            return left.equals(right) ? SlartiBoolean.TRUE : SlartiBoolean.FALSE;
        }
    }),
    /**
     * Logical and of booleans.
     */
    AND(new SlartiBuiltInFunctio("and") {
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
    OR(new SlartiBuiltInFunctio("or") {
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
    PRINTLN(new SlartiBuiltInFunctio("println") {
        @Override
        public final SlartiNode apply(final List<SlartiNode> args) {
            final StringBuilder buffer = new StringBuilder();

            for (final SlartiNode arg : args) {
                buffer.append(arg.castToString().value());
            }

            io.println(buffer.toString());
            return SlartiList.EMPTY;
        }
    }),
    /**
     * Prints the given arguments.
     */
    PRINT(new SlartiBuiltInFunctio("print") {
        @Override
        public final SlartiNode apply(final List<SlartiNode> args) {
            final StringBuilder buffer = new StringBuilder();

            for (final SlartiNode arg : args) {
                buffer.append(arg.castToString().value());
            }

            io.print(buffer.toString());
            return SlartiList.EMPTY;
        }
    }),
    /**
     * Generates a random integer.
     */
    RANDOM(new SlartiBuiltInFunctio("random") {
        private final Random r = new Random();

        @Override
        public SlartiNode apply(final List<SlartiNode> args) {
            return new SlartiInteger(r.nextLong());
        }
    });

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
            ((SlartiBuiltInFunctio) fn.impl()).setIo(io);
            env.putValue(fn.impl.name(), fn.impl);
        }
    }

    private static abstract class SlartiBuiltInFunctio extends SlartiFunction {
        protected IO io;
        protected SlartiBuiltInFunctio(String name) {
            super(name);
        }

        public final boolean isBuiltIn() {
            return true;
        }

        protected void setIo(final IO io) {
            this.io = Validate.notNull(io, "io");
        }

        protected SlartiError unsupportedTypeOfArgument(final SlartiNode<?> argument) {
            return new SlartiError(
                String.format("Unsupported type %s of argument for function %s!",
                    argument.getClass().getSimpleName(), name()));
        }
    }
}
