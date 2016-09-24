package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiInteger;
import de.weltraumschaf.slartibartfass.node.type.SlartiReal;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.Matchers.closeTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static de.weltraumschaf.slartibartfass.node.Slarti.*;

public class SlartiBuiltinFunctions_SubtractTest {

    private static final double DELTA = 0.0001d;

    @Test
    public void name() {
        assertThat(SlartiBuiltinFunctions.SUBTRACT.impl().name(), is("-"));
    }

    @Test(expected = RuntimeException.class)
    public void eval_zeroArgs() {
        SlartiBuiltinFunctions.SUBTRACT.impl().apply(Collections.emptyList());
    }

    @Test
    public void eval_oneArgs() {
        assertThat(SlartiBuiltinFunctions.SUBTRACT.impl().apply(of(23L)), is(new SlartiInteger(-23L)));
        assertThat(SlartiBuiltinFunctions.SUBTRACT.impl().apply(of(23d)), is(new SlartiReal(-23d)));
    }

    @Test
    public void eval_twoArgs() {
        final SlartiNode result = SlartiBuiltinFunctions.SUBTRACT.impl().apply(of(23L), of(42L));

        assertThat(result, is(new SlartiInteger(-19L)));
    }

    @Test
    public void eval_threeArgs() {
        final SlartiNode result = SlartiBuiltinFunctions.SUBTRACT.impl().apply(of(23L), of(42L), of(2L));

        assertThat(result, is(new SlartiInteger(-21L)));
    }

    @Test
    public void eval_onlyReals() {
        final SlartiNode result = SlartiBuiltinFunctions.SUBTRACT.impl().apply(of(3.14d), of(2d), of(-1d));

        assertThat(((SlartiReal) result).value(), is(closeTo(2.14d, DELTA)));
    }

    @Test
    public void eval_integersAndRealsMixed() {
        SlartiNode result = SlartiBuiltinFunctions.SUBTRACT.impl().apply(of(3.14d), of(1L), of(-2d));

        assertThat(((SlartiReal) result).value(), is(closeTo(4.14d, DELTA)));

        result = SlartiBuiltinFunctions.SUBTRACT.impl().apply(of(1L), of(3.14d), of(-2L));

        assertThat(((SlartiReal) result).value(), is(closeTo(-0.14d, DELTA)));
    }
}
