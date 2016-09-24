package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.slartibartfass.node.type.SlartiInteger;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class SlartiBuiltinFunctions_ModuloTest {
    @Test
    public void name() {
        assertThat(SlartiBuiltinFunctions.MODULO.impl().name(), is("%"));
    }

    @Test(expected = RuntimeException.class)
    public void eval_zeroArgs() {
        SlartiBuiltinFunctions.MODULO.impl().apply(Collections.emptyList());
    }

    @Test(expected = RuntimeException.class)
    public void eval_oneArgs() {
        SlartiBuiltinFunctions.MODULO.impl().apply(Collections.singletonList(new SlartiInteger(23L)));
    }

    @Test
    public void eval_twoArgs() {
        final Object result = SlartiBuiltinFunctions.MODULO.impl()
            .apply(Arrays.asList(new SlartiInteger(55L), new SlartiInteger(10L)));

        assertThat(result, is(new SlartiInteger(5L)));
    }

    @Test
    public void eval_threeArgs() {
        final Object result = SlartiBuiltinFunctions.MODULO.impl()
            .apply(Arrays.asList(new SlartiInteger(55L), new SlartiInteger(10L), new SlartiInteger(2L)));

        assertThat(result, is(new SlartiInteger(1L)));
    }

}
