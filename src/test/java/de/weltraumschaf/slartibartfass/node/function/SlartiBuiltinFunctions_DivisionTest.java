package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.slartibartfass.node.type.SlartiInteger;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class SlartiBuiltinFunctions_DivisionTest {
    @Test
    public void name() {
        assertThat(SlartiBuiltinFunctions.DIVISION.impl().name(), is("/"));
    }

    @Test(expected = RuntimeException.class)
    public void eval_zeroArgs() {
        SlartiBuiltinFunctions.DIVISION.impl().apply(Collections.emptyList());
    }

    @Test(expected = RuntimeException.class)
    public void division_oneArgs() {
        SlartiBuiltinFunctions.DIVISION.impl().apply(Collections.singletonList(new SlartiInteger(23L)));
    }

    @Test
    public void eval_twoArgs() {
        final Object result = SlartiBuiltinFunctions.DIVISION.impl()
            .apply(Arrays.asList(new SlartiInteger(55L), new SlartiInteger(10L)));

        assertThat(result, is(new SlartiInteger(5L)));
    }

    @Test
    public void eval_threeArgs() {
        final Object result = SlartiBuiltinFunctions.DIVISION.impl()
            .apply(Arrays.asList(new SlartiInteger(55L), new SlartiInteger(10L), new SlartiInteger(2L)));

        assertThat(result, is(new SlartiInteger(2L)));
    }

}
