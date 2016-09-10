package de.weltraumschaf.slartibartfass.node;

import de.weltraumschaf.slartibartfass.Environment;

import java.io.PrintStream;
import java.util.List;

public enum SlartiBuiltinFunction {
    PLUS(new SlartiFunction("+") {
        @Override
        public final Object apply(final List<Object> args) {
            long sum = 0;

            for (final Object arg : args) {
                sum += Long.parseLong(arg.toString());
            }

            return Long.valueOf(sum);
        }
    }),
    MINUS(new SlartiFunction("-") {
        @Override
        public final Object apply(final List<Object> args) {
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
    MULTIPLY(new SlartiFunction("*") {
        @Override
        public final Object apply(final List<Object> args) {
            long result = 1;

            for (final Object arg : args) {
                result *= Long.parseLong(arg.toString());
            }

            return Long.valueOf(result);
        }
    }),
    DIVISION(new SlartiFunction("/") {
        @Override
        public final Object apply(final List<Object> args) {
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
    MODULO(new SlartiFunction("%") {
        @Override
        public final Object apply(final List<Object> args) {
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
    LESS_THAN(new SlartiFunction("<") {
        @Override
        public Object apply(final List<Object> args) {
            if (args.size() != 2) {
                throw new RuntimeException(String.format("Function %s requires two arguments!", this.name()));
            }

            final long left = Long.parseLong(args.get(0).toString());
            final long right = Long.parseLong(args.get(1).toString());

            return left < right;
        }
    }),
    GREATER_THAN(new SlartiFunction(">") {
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
    EQUALS(new SlartiFunction("=") {
        @Override
        public Object apply(List<Object> args) {
            if (args.size() != 2) {
                throw new RuntimeException(String.format("Function %s requires two arguments!", this.name()));
            }

            final Object left = args.get(0);
            final Object right = args.get(1);

            return left.equals(right);
        }
    }),
    PRINTLN(new SlartiFunction("println") {
        @Override
        public final Object apply(final List<Object> args) {
            for (final Object arg : args) {
                System.out.println(arg.toString());
            }

            return SlartiList.EMPTY;
        }
    });

    private final SlartiFunction impl;

    SlartiBuiltinFunction(SlartiFunction impl) {
        this.impl = impl;
    }

    public SlartiFunction impl() {
        return impl;
    }

    public static void register(final Environment env) {
        for (final SlartiBuiltinFunction fn : values()) {
            env.putValue(fn.impl.name(), fn.impl);
        }
    }
}
