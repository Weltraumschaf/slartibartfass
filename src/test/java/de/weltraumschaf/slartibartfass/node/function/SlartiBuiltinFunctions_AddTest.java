package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.slartibartfass.node.SlartiNode;
import de.weltraumschaf.slartibartfass.node.type.SlartiReal;
import org.junit.Test;

import java.util.Collections;

import static org.hamcrest.Matchers.closeTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static de.weltraumschaf.slartibartfass.node.Slarti.*;

public class SlartiBuiltinFunctions_AddTest {

    private static final double DELTA = 0.0001d;

    @Test
    public void name() {
        assertThat(SlartiBuiltinFunctions.ADD.impl().symbol(), is(sym("+")));
    }

    @Test
    public void eval_zeroArgs() {
        final SlartiNode result = SlartiBuiltinFunctions.ADD.impl().apply(Collections.emptyList());

        assertThat(result, is(of(0L)));
    }

    @Test
    public void eval_oneArgs() {
        final SlartiNode result = SlartiBuiltinFunctions.ADD.impl().apply(of(23L));

        assertThat(result, is(of(23L)));
    }

    @Test
    public void eval_twoArgs() {
        final SlartiNode result = SlartiBuiltinFunctions.ADD.impl().apply(of(23L), of(42L));

        assertThat(result, is(of(65L)));
    }

    @Test
    public void eval_threeArgs() {
        final SlartiNode result = SlartiBuiltinFunctions.ADD.impl().apply(of(23L), of(42L), of(2L));

        assertThat(result, is(of(67L)));
    }

    @Test
    public void eval_onlyReals() {
        final SlartiNode result = SlartiBuiltinFunctions.ADD.impl().apply(of(3.14d), of(1d), of(-2d));

        assertThat(((SlartiReal) result).value(), is(closeTo(2.14d, DELTA)));
    }

    @Test
    public void eval_integersAndRealsMixed() {
        SlartiNode result = SlartiBuiltinFunctions.ADD.impl().apply(of(3.14d), of(1L), of(-2d));

        assertThat(((SlartiReal) result).value(), is(closeTo(2.14d, DELTA)));

        result = SlartiBuiltinFunctions.ADD.impl().apply(of(1L), of(3.14d), of(-2L));

        assertThat(((SlartiReal) result).value(), is(closeTo(2.14d, DELTA)));
    }

}
