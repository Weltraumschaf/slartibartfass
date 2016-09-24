package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiInteger;
import de.weltraumschaf.slartibartfass.node.type.SlartiReal;
import org.junit.Test;

import java.util.Collections;

import static org.hamcrest.Matchers.closeTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static de.weltraumschaf.slartibartfass.node.Slarti.*;

public class SlartiBuiltinFunctions_MultiplyTest {
    private static final double DELTA = 0.0001d;

    @Test
    public void name() {
        assertThat(SlartiBuiltinFunctions.MULTIPLY.impl().name(), is("*"));
    }

    @Test
    public void eval_zeroArgs() {
        final Object result = SlartiBuiltinFunctions.MULTIPLY.impl().apply(Collections.emptyList());

        assertThat(result, is(of(1L)));
    }

    @Test
    public void eval_oneArgs() {
        assertThat(SlartiBuiltinFunctions.MULTIPLY.impl().apply(of(23L)), is(new SlartiInteger(23L)));
        assertThat(SlartiBuiltinFunctions.MULTIPLY.impl().apply(of(23d)), is(new SlartiReal(23d)));
    }

    @Test
    public void eval_twoArgs() {
        final SlartiNode result = SlartiBuiltinFunctions.MULTIPLY.impl().apply(of(10L), of(23L));

        assertThat(result, is(of(230L)));
    }

    @Test
    public void eval_threeArgs() {
        final SlartiNode result = SlartiBuiltinFunctions.MULTIPLY.impl().apply(of(10L), of(23L), of(2L));

        assertThat(result, is(of(460L)));
    }

    @Test
    public void eval_onlyReals() {
        final SlartiNode result = SlartiBuiltinFunctions.MULTIPLY.impl().apply(of(2.5d), of(2.5d), of(0.5d));

        assertThat(((SlartiReal) result).value(), is(closeTo(3.125d, DELTA)));
    }

    @Test
    public void eval_integersAndRealsMixed() {
        SlartiNode result = SlartiBuiltinFunctions.MULTIPLY.impl().apply(of(2.5d), of(2L), of(0.5d));

        assertThat(((SlartiReal) result).value(), is(closeTo(2.5d, DELTA)));

        result = SlartiBuiltinFunctions.MULTIPLY.impl().apply(of(2L), of(2.5d), of(3L));

        assertThat(((SlartiReal) result).value(), is(closeTo(15d, DELTA)));
    }
}
