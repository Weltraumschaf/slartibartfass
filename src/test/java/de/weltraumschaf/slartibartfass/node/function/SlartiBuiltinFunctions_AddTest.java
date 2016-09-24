package de.weltraumschaf.slartibartfass.node.function;

import de.weltraumschaf.slartibartfass.node.type.SlartiInteger;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class SlartiBuiltinFunctions_AddTest {

    @Test
    public void name() {
        assertThat(SlartiBuiltinFunctions.ADD.impl().name(), is("+"));
    }

    @Test
    public void eval_zeroArgs() {
        final Object result = SlartiBuiltinFunctions.ADD.impl().apply(Collections.emptyList());

        assertThat(result, is(new SlartiInteger(0L)));
    }

    @Test
    public void eval_oneArgs() {
        final Object result = SlartiBuiltinFunctions.ADD.impl().
            apply(Collections.singletonList(new SlartiInteger(23L)));

        assertThat(result, is(new SlartiInteger(23L)));
    }

    @Test
    public void eval_twoArgs() {
        final Object result = SlartiBuiltinFunctions.ADD.impl()
            .apply(Arrays.asList(new SlartiInteger(23L), new SlartiInteger(42L)));

        assertThat(result, is(new SlartiInteger(65L)));
    }

    @Test
    public void eval_threeArgs() {
        final Object result = SlartiBuiltinFunctions.ADD.impl()
            .apply(Arrays.asList(new SlartiInteger(23L), new SlartiInteger(42L), new SlartiInteger(2L)));

        assertThat(result, is(new SlartiInteger(67L)));
    }

}
