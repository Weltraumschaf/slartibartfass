package de.weltraumschaf.slartibartfass;

import de.weltraumschaf.slartibartfass.node.SlartiNode;
import org.junit.Ignore;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.List;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

public class ApplicationTest {
//    private final Application sut = new Application(new String[0]);

    @Test
    public void eval_simpleAddition() throws IOException {
        final String src = "(+ 3 4)";

//        assertThat(sut.eval(readSource(src)), is(7L));
    }

    @Test
    public void eval_largerMathExpression() throws IOException {
        final String src = "(+ (* 3 2) (- 10 4))";

//        assertThat(sut.eval(readSource(src)), is(12L));
    }

    @Test
    public void eval_fibonacci_recursive() throws IOException {
        final String src = "(define fib\n" +
            "  (lambda (n)\n" +
            "    (if (< n 2)\n" +
            "        1\n" +
            "        (+ (fib (- n 1))\n" +
            "           (fib (- n 2))))))\n" +
            "\n" +
            "(fib 10)";

//        assertThat(sut.eval(readSource(src)), is(89L)); // This is for 11, should be 55.
    }

    @Test
    @Ignore
    public void eval_fibonacci_iterative() throws IOException {
        final String src = "(define fibonacci\n" +
            "  (lambda (n)\n" +
            "    (define iter\n" +
            "      (lambda (i n1 n2)\n" +
            "        (if (= i 0)\n" +
            "            n2\n" +
            "            (iter (- i 1)\n" +
            "                  n2\n" +
            "                  (+ n1 n2)))))\n" +
            "    (iter n 0 1)))";

//        assertThat(sut.eval(readSource(src)), is(55L));
    }
}
