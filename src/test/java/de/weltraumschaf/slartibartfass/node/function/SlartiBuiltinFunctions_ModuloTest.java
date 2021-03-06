package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.slartibartfass.SlartiError;
import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiReal;
import org.junit.Test;

import java.util.Collections;

import static org.hamcrest.Matchers.closeTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static de.weltraumschaf.slartibartfass.node.Slarti.*;

/**
 * Tests for {@link SlartiBuiltinFunctions#MODULO}.
 *
 * @author Sven Strittmatter
 */
public class SlartiBuiltinFunctions_ModuloTest {
    private static final double DELTA = 0.0001d;

    @Test
    public void name() {
        assertThat(SlartiBuiltinFunctions.MODULO.impl().symbol(), is(sym("%")));
    }

    @Test(expected = RuntimeException.class)
    public void eval_zeroArgs() {
        SlartiBuiltinFunctions.MODULO.impl().apply(Collections.emptyList());
    }

    @Test(expected = RuntimeException.class)
    public void eval_oneArgs() {
        SlartiBuiltinFunctions.MODULO.impl().apply(of(23L));
    }

    @Test
    public void eval_twoArgs() {
        final SlartiNode result = SlartiBuiltinFunctions.MODULO.impl().apply(of(55L), of(10L));

        assertThat(result, is(of(5L)));
    }

    @Test
    public void eval_threeArgs() {
        final SlartiNode result = SlartiBuiltinFunctions.MODULO.impl().apply(of(55L), of(10L), of(2L));

        assertThat(result, is(of(1L)));
    }

    @Test
    public void eavl_real() {
        final SlartiNode result = SlartiBuiltinFunctions.MODULO.impl().apply(of(3.14), of(2d));

        assertThat(((SlartiReal) result).value(), is(closeTo(1.14d, DELTA)));
    }

    @Test
    public void eval_mixed() {
        final SlartiNode result = SlartiBuiltinFunctions.MODULO.impl().apply(of(3.14), of(2L));

        assertThat(((SlartiReal) result).value(), is(closeTo(1.14d, DELTA)));
    }

    @Test(expected = SlartiError.class)
    public void eval_divisionByZero() {
        SlartiBuiltinFunctions.MODULO.impl().apply(of(3.14), of(0L));
    }

}
