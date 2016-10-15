package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.commons.validate.Validate;
import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiInteger;
import de.weltraumschaf.slartibartfass.node.type.SlartiReal;

import java.util.function.BiFunction;

import static de.weltraumschaf.slartibartfass.node.Slarti.*;

/**
 * Helper class to accumulate the result of arithmetic operations.
 * <p>
 * This class does the job to check if the result should end in an integer or real number result.
 * </p>
 *
 * @author Sven Strittmatter
 */
final class ResultAccumulator {

    /**
     * Operation for integer numbers.
     */
    private final BiFunction<Long, Long, Long> integerOperation;
    /**
     * Operations for real numbers.
     */
    private final BiFunction<Double, Double, Double> realOperation;
    /**
     * Whether there was a real number operand.
     */
    private boolean realSeen;
    /**
     * Accumulates the integer result until the first real operand occurs.
     */
    private long integerResult;
    /**
     * Acumulates the real result after the first real operand occurred.
     */
    private double realResult;

    /**
     * Dedicated constructor.
     *
     * @param integerOperation must not be {@code null}
     * @param realOperation    must not be {@code null}
     */
    ResultAccumulator(final BiFunction<Long, Long, Long> integerOperation, final BiFunction<Double, Double, Double> realOperation) {
        super();
        this.integerOperation = Validate.notNull(integerOperation, "integerOperation");
        this.realOperation = Validate.notNull(realOperation, "realOperation");
    }

    /**
     * Initializes the accumulator with a result on which the operands will be applied.
     *
     * @param operand must not be {@code null}
     */
    void init(final SlartiNode operand) {
        Validate.notNull(operand, "operand");

        if (operand.isInteger()) {
            integerResult = operand.castToInteger().value();
            realSeen = false;
        } else if (operand.isReal()) {
            realResult = operand.castToReal().value();
            realSeen = true;
        }
    }

    /**
     * Apply an operand to the accumulator.
     * <p>
     * Throws an {@link SlartiError} if the operand is neither of type {@link SlartiInteger} nor {@link SlartiReal}.
     * </p>
     *
     * @param operand must not be {@code null}
     */
    void apply(final SlartiNode operand) {
        Validate.notNull(operand, "operand");

        if (operand.isInteger()) {
            if (realSeen) {
                realResult = realOperation.apply(realResult, operand.castToReal().value());
            } else {
                integerResult = integerOperation.apply(integerResult, operand.castToInteger().value());
            }
        } else if (operand.isReal()) {
            if (realSeen) {
                realResult = realOperation.apply(realResult, operand.castToReal().value());
            } else {
                realResult = realOperation.apply((double) integerResult, operand.castToReal().value());
                realSeen = true;
            }
        }
    }

    /**
     * Returns either an {@link SlartiInteger} or {@link SlartiReal} depending on the applied operands.
     *
     * @return never {@code null}
     */
    SlartiNode result() {
        return realSeen ? of(realResult) : of(integerResult);
    }
}
