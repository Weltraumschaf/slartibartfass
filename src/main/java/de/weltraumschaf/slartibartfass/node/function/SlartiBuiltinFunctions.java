package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.type.SlartiInteger;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;

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
        public final Object apply(final List<Object> args) {
            // FIXME Deal with real numbers.
            long sum = 0;

            for (final Object arg : args) {
                sum += Long.parseLong(arg.toString());
            }

            return Long.valueOf(sum);
        }
    }),
    /**
     * Number subtraction.
     */
    SUBTRACT(new SlartiBuiltInFunctio("-") {
        @Override
        public final Object apply(final List<Object> args) {
            // FIXME Deal with real numbers.
            if (args.size() < 1) {
                throw new RuntimeException(String.format("Function %s requires at least one argument!", this.name()));
            } else if (args.size() == 1) {
                return -((Long) args.get(0));
            } else {
                long result = (Long) args.get(0);

                for (int i=1; i<args.size(); i++) {
                    result -= (Long) args.get(i);
                }

                return result;
            }
        }
    }),
    /**
     * Number multiplication.
     */
    MULTIPLY(new SlartiBuiltInFunctio("*") {
        @Override
        public final Object apply(final List<Object> args) {
            // FIXME Deal with real numbers.
            long result = 1;

            for (final Object arg : args) {
                result *= Long.parseLong(arg.toString());
            }

            return Long.valueOf(result);
        }
    }),
    /**
     * Number division.
     */
    DIVISION(new SlartiBuiltInFunctio("/") {
        @Override
        public final Object apply(final List<Object> args) {
            // FIXME Deal with real numbers.
            if (args.size() < 2) {
                throw new RuntimeException(String.format("Function %s requires at least two arguments!", this.name()));
            }

            long result = Long.parseLong(args.get(0).toString());

            for (final Object arg : args.subList(1, args.size())) {
                result /= Long.parseLong(arg.toString());
            }

            return result;
        }
    }),
    /**
     * Integer modulo.
     */
    MODULO(new SlartiBuiltInFunctio("%") {
        @Override
        public final Object apply(final List<Object> args) {
            // FIXME Deal with real numbers.
            if (args.size() < 2) {
                throw new RuntimeException(String.format("Function %s requires at least two arguments!", this.name()));
            }

            long result = Long.parseLong(args.get(0).toString());

            for (final Object arg : args.subList(1, args.size())) {
                result %= Long.parseLong(arg.toString());
            }

            return result;
        }
    }),
    /**
     * Less than number comparison.
     */
    LESS_THAN(new SlartiBuiltInFunctio("<") {
        @Override
        public Object apply(final List<Object> args) {
            // FIXME Deal with real numbers.
            if (args.size() != 2) {
                throw new RuntimeException(String.format("Function %s requires two arguments!", this.name()));
            }

            final long left = Long.parseLong(args.get(0).toString());
            final long right = Long.parseLong(args.get(1).toString());

            return left < right;
        }
    }),
    /**
     * Greater than number comparison.
     */
    GREATER_THAN(new SlartiBuiltInFunctio(">") {
        // FIXME Deal with real numbers.
        @Override
        public Object apply(List<Object> args) {
            if (args.size() != 2) {
                throw new RuntimeException(String.format("Function %s requires two arguments!", this.name()));
            }

            final long left = Long.parseLong(args.get(0).toString());
            final long right = Long.parseLong(args.get(1).toString());

            return left > right;
        }
    }),
    /**
     * Equals comparison.
     */
    EQUALS(new SlartiBuiltInFunctio("=") {
        @Override
        public Object apply(List<Object> args) {
            // FIXME Deal with real numbers.
            if (args.size() != 2) {
                throw new RuntimeException(String.format("Function %s requires two arguments!", this.name()));
            }

            final Object left = args.get(0);
            final Object right = args.get(1);

            return left.equals(right);
        }
    }),
    /**
     * Logical and of booleans.
     */
    AND(new SlartiBuiltInFunctio("and") {
        @Override
        public Object apply(final List<Object> args) {
            if (args == null || args.isEmpty()) {
                return Boolean.FALSE;
            }

            for (final Object arg : args) {
                if (!Boolean.parseBoolean(arg.toString())) {
                    return Boolean.FALSE;
                }
            }

            return Boolean.TRUE;
        }
    }),
    /**
     * Logical or of booleans.
     */
    OR(new SlartiBuiltInFunctio("or") {
        @Override
        public Object apply(final List<Object> args) {
            if (args == null || args.isEmpty()) {
                return Boolean.FALSE;
            }

            for (final Object arg : args) {
                if (Boolean.parseBoolean(arg.toString())) {
                    return Boolean.TRUE;
                }
            }

            return Boolean.FALSE;
        }
    }),
    /**
     * Prints the given arguments appended with a newline.
     */
    PRINTLN(new SlartiBuiltInFunctio("println") {
        @Override
        public final Object apply(final List<Object> args) {
            final StringBuilder buffer = new StringBuilder();

            for (final Object arg : args) {
                buffer.append(arg.toString());
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
        public final Object apply(final List<Object> args) {
            final StringBuilder buffer = new StringBuilder();

            for (final Object arg : args) {
                buffer.append(arg.toString());
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
        public Object apply(final List<Object> args) {
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
    }
}
