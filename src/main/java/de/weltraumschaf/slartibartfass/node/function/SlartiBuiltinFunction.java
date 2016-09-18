package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.commons.application.IO;
import de.weltraumschaf.slartibartfass.Environment;
import de.weltraumschaf.slartibartfass.node.type.SlartiList;

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
    AND(new SlartiFunction("and") {
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
    OR(new SlartiFunction("or") {
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
    PRINTLN(new SlartiFunction("println") {
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
    PRINT(new SlartiFunction("print") {
        @Override
        public final Object apply(final List<Object> args) {
            final StringBuilder buffer = new StringBuilder();

            for (final Object arg : args) {
                buffer.append(arg.toString());
            }

            io.print(buffer.toString());
            return SlartiList.EMPTY;
        }
    });

    private static IO io;

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

    public static void setIo(final IO io) {
        SlartiBuiltinFunction.io = io;
    }
}
